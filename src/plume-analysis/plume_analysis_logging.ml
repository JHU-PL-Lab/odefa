type plume_logging_level =
  | Log_nothing
  | Log_result
  | Log_everything
  [@@deriving eq, ord, show, to_yojson]
;;

type plume_analysis_logging_config =
  { plume_json_logger : Yojson.Safe.t -> unit
  ; plume_cfg_logging_level : plume_logging_level
  ; plume_pdr_logging_level : plume_logging_level
  ; plume_pdr_deltas : bool
  }
  [@@deriving show]
;;
