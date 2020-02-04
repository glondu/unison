(* Unison file synchronizer: src/osx.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

val init : bool -> unit
val isMacOSX : bool

val rsrc : bool Prefs.t

type 'a ressInfo [@@deriving protobuf]
type ressUnit = int [@@deriving protobuf]
type ressStamp = ressUnit ressInfo [@@deriving protobuf]
type info =
  { ressInfo : (Fspath.t * int64) ressInfo;
    finfo : string } [@@deriving protobuf]

val defaultInfos :  [> `DIRECTORY | `FILE ] -> info

val getFileInfos : Fspath.t -> Path.local -> [> `DIRECTORY | `FILE ] -> info
val setFileInfos : Fspath.t -> Path.local -> string -> unit

val ressUnchanged :
  'a ressInfo -> 'b ressInfo -> float option -> bool -> bool

val ressFingerprint : Fspath.t -> Path.local -> info -> Fingerprint.t
val ressLength : 'a ressInfo -> Uutil.Filesize.t

val ressDummy : ressStamp
val ressStampToString : ressStamp -> string

val stamp : info -> ressStamp

val openRessIn : Fspath.t -> Path.local -> in_channel
val openRessOut : Fspath.t -> Path.local -> Uutil.Filesize.t -> out_channel
