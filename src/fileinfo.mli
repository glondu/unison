(* Unison file synchronizer: src/fileinfo.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

type typ = [`ABSENT | `FILE | `DIRECTORY | `SYMLINK] [@@deriving protobuf]
val type2string : typ -> string

type t = { typ : typ; inode : int; desc : Props.t; osX : Osx.info} [@@deriving protobuf]

val get : bool (* fromRoot *) -> Fspath.t -> Path.local -> t
val set : Fspath.t -> Path.local ->
          [`Set of Props.t | `Copy of Path.local | `Update of Props.t] ->
          Props.t -> unit
val get' : System.fspath -> t

(* IF THIS CHANGES, MAKE SURE TO INCREMENT THE ARCHIVE VERSION NUMBER!       *)
type stamp =
    InodeStamp of int [@key 1]        (* inode number, for Unix systems *)
  | CtimeStamp of float [@key 2]      (* creation time, for windows systems *)
[@@deriving protobuf]

val stamp : t -> stamp

val ressStamp : t -> Osx.ressStamp

(* Check whether a file is unchanged *)
val unchanged : Fspath.t -> Path.local -> t -> (t * bool * bool)

(****)

val init : bool -> unit
val allowSymlinks : [`True|`False|`Default] Prefs.t
val ignoreInodeNumbers : bool Prefs.t
