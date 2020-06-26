let major_version = 0;;

let minor_version = 0;;

let patch_version = 0;;

let codenames =
  ["Raava"; "Wan"; "Yangchen"; "Kuruk"; "Kyoshi"; "Roku"; "Aang"; "Korra"]

let version_str =
  "Version " ^
  (string_of_int major_version) ^ "." ^
  (string_of_int minor_version) ^ "." ^
  (string_of_int patch_version) ^
  match List.nth_opt codenames major_version with
  | Some(name) -> " " ^ name
  | None -> ""
;;