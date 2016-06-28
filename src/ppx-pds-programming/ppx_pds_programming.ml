open Pds_programming

let () =
  Ppx_utils.run_mappers [dummy_mapper; double_mapper]
;;
