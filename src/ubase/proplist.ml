(* Unison file synchronizer: src/ubase/proplist.ml *)
(* Copyright 1999-2020, Benjamin C. Pierce

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

type 'a key = string
type t = Obj.t Util.StringMap.t

let names = ref Util.StringMap.empty

let register nm bin =
  if (Util.StringMap.mem nm !names) then
    raise (Util.Fatal
            (Format.sprintf "Property lists: %s already registered!" nm));
  names := Util.StringMap.add nm (Obj.repr bin) !names;
  nm

let empty = Util.StringMap.empty

let mem = Util.StringMap.mem

let find (k : 'a key) m : 'a = Obj.obj (Util.StringMap.find k m)

let add (k : 'a key) (v : 'a) m = Util.StringMap.add k (Obj.repr v) m

let find_bin (k : 'a key) : 'a Bin_prot.Type_class.t =
  match Util.StringMap.find_opt k !names with
  | Some x -> Obj.obj x
  | None ->
     raise (Util.Fatal (Format.sprintf "Property lists: %s not yet registered!" k))

open Bin_prot
open Std

module Proplist_spec = struct
  type t = Obj.t Util.StringMap.t
  type bigstring = Common.buf
  type el = string * bigstring [@@deriving bin_io]
  let caller_identity = Shape.Uuid.of_string "dd331c89-17ad-4027-9a31-c6ea04d738f4"
  let module_name = Some "Proplist"
  let length = Util.StringMap.cardinal
  let iter xs ~f =
    Util.StringMap.iter
      (fun k obj ->
        let bin = find_bin k in
        let v = Obj.obj obj in
        let size = bin.writer.size v in
        let buf = Common.create_buf size in
        ignore (bin.writer.write buf ~pos:0 v);
        f (k, buf)
      ) xs
  let init ~len ~next =
    let res = ref Util.StringMap.empty in
    for i = 1 to len do
      let k, buf = next () in
      let bin = find_bin k in
      let v = bin.reader.read buf ~pos_ref:(ref 0) in
      res := Util.StringMap.add k (Obj.repr v) !res
    done;
    !res
end

include Utils.Make_iterable_binable (Proplist_spec)
