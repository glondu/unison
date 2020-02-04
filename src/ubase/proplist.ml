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

let register nm p =
  if (Util.StringMap.mem nm !names) then
    raise (Util.Fatal
            (Format.sprintf "Property lists: %s already registered!" nm));
  names := Util.StringMap.add nm (Obj.repr p) !names;
  nm

let empty = Util.StringMap.empty

let mem = Util.StringMap.mem

let find (k : 'a key) m : 'a = Obj.obj (Util.StringMap.find k m)

let add (k : 'a key) (v : 'a) m = Util.StringMap.add k (Obj.repr v) m

let find_protobuf (k : 'a key) :
      ('a -> Protobuf.Encoder.t -> unit) * (Protobuf.Decoder.t -> 'a)
  =
  match Util.StringMap.find_opt k !names with
  | Some x -> Obj.obj x
  | None ->
     raise (Util.Fatal (Format.sprintf "Property lists: %s not yet registered!" k))

type proplist = (string * bytes) list [@@deriving protobuf]

let to_protobuf x e =
  proplist_to_protobuf
    (Util.StringMap.fold
       (fun k v accu ->
         let encoder, _ = find_protobuf k in
         (k, Protobuf.Encoder.encode_exn encoder v) :: accu
       ) x []
    ) e

let from_protobuf d =
  List.fold_left
    (fun accu (k, v) ->
      let _, decoder = find_protobuf k in
      let v = Protobuf.Decoder.decode_exn decoder v in
      Util.StringMap.add k (Obj.repr v) accu
    ) Util.StringMap.empty (proplist_from_protobuf d)
