(* open Core *)
open Shexp_process
open Shexp_process.Infix
(* open Shexp_process.Logged *)

let testcases = [
  "input_blur.natodefa";
  "input_eta.natodefa";
  "input_facehugger.natodefa";
  "input_flatten.natodefa";
  "input_k_cfa_2.natodefa";
  "input_k_cfa_3.natodefa";
  "input_map.natodefa";
  "input_mj09.natodefa";
  "input_sat_1.natodefa";
  "smbc_fold0s.natodefa";
  "smbc_gen_list_len.natodefa";
  "smbc_long_rev_sum3.natodefa";
  "smbc_pigeon.natodefa";
  "smbc_sorted_sum.natodefa";
]

let repeat_n = 3

let working_path = "result/working/"

let timeout_limit = "20m"

let make_result_file () = 
  (run "date" ["-u"; "+%Y_%m_%d_%H_%M_%S"])
  |- (run "xargs" ["-I"; "%"; "mkdir"; "-p";"result/%"])

let prepare : unit t = 
  run "rm" ["-rf"; working_path] >>
  run "mkdir" ["-p"; working_path]
(* make_result_file () *)

let benchmark test n : unit t = 
  (* let test = "input_map.natodefa" in *)
  let test_path = "benchmark-test-generation/cases/" ^ test
  and test_result = working_path ^ (test ^ "_" ^ (string_of_int n)) ^ ".txt" 
  and test_time_result = working_path ^ test ^ ".time.txt" in
  print test
  (* time gtimeout --foreground 1m ls *)
  >> stdout_to ~append:() test_result (call [
      "/usr/bin/time"; 
      "-o"; test_time_result; 
      "-a";
      "-f"; "%e\n%Uuser %Ssystem %Eelapsed %PCPU (%Xtext+%Ddata %Mmax)k %Iinputs+%Ooutputs (%Fmajor+%Rminor)pagefaults %Wswaps";
      "/usr/bin/timeout"; 
      "--foreground"; timeout_limit;
      "./test_generator"; 
      "-t"; "target"; 
      "-r"; "1"; 
      "-b"; "true"; 
      test_path])
  >> echo @@ " done - " ^ (string_of_int n)
(* |- run "tee" ["-a"; result_file] *)

let extra_case : unit t =
  let extra_result = working_path ^ "extra.txt"
  and file_path = "benchmark-test-generation/other_cases/input_list_sum_add_build.natodefa" in
  stdout_to ~append:() extra_result (
    call ["./test_generator"; file_path; "-t"; "target"; "-r"; "4" ]
  )
  >> echo @@ " done - extra"


let stat : unit t =
  let table = working_path ^ "0table.txt" in
  List.iter testcases ~f:(fun testcase ->
      stdout_to ~append:() table
        (run "echo" [testcase]
         >> run "awk" ["BEGIN {sum=0;count=0} {if (NR%2==1) {sum=sum+$1; count=count+1}}  END {print sum/count} "; working_path ^ testcase ^ ".time.txt"]
         >> run "awk" ["FNR == 2"; working_path ^ testcase ^ "_1.txt"]
         (* >> run "awk" ["BEGIN {sum=0;count=0} {FNR==NR; if (NR==2) {sum=sum+$1; count=count+1}} {next} {print sum/count}' result/evaluate-2020-05-22-23:55:15Z/*.result.txt"] *)
        )
    )

let collect_result : unit t =
  (* let awk_job = "awk 'BEGIN {sum=0;count=0} {if (NR%2==1) {sum=sum+$1;count=count+1}}  END {print sum/count}' $f"
     in  
     in (run ("for f in " ^ working_path ^ "*.time.txt; do " ^ awk_job ^ "; done") [])
     (run "./script/echo.sh" [])
     >>  *)
  (* let test_name = String.(sub testcase 0 (rindex testcase '.')) in *)  
  (run "date" ["-u"; "+%Y-%m-%d-%H:%M:%SZ"])
  |- (run "xargs" ["-I"; "%"; "mv"; working_path; "result/evaluate-%"])

let rec countdown n task : unit t =
  if n > 0 then
    task n
    >> countdown (n-1) task
  else
    echo "done"

let main  =
  ignore @@ 
  eval (
    prepare
    >> List.iter testcases ~f:(fun task -> countdown repeat_n (benchmark task))
    >> stat
    >> extra_case
    >> collect_result)