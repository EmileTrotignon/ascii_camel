let comment_open = Re.(seq [bol; repn notnl 0 (Some 3)])

let comment_close = Re.(seq [repn notnl 0 (Some 1)])

let normal_header =
  Re.(
    compile
    @@ seq
         [ group comment_open
         ; str
             {|**************************************************************************|}
         ; group comment_close; eol
         ; str "\n"
         ; comment_open
         ; str
             {|*                                                                        *|}
         ; comment_close
         ; str "\n"
         ; comment_open
         ; str
             {|*                                 OCaml                                  *|}
         ; comment_close
         ; str "\n"
         ; comment_open
         ; str
             {|*                                                                        *|}
         ; comment_close
         ; str "\n" ] )

let replace_header str =
  Re.replace normal_header
    ~f:(fun group ->
      let comment_open = Re.Group.get group 1
      and comment_close = Re.Group.get group 2 in
      Printf.printf "open %S close %S\n" comment_open comment_close ;
      Printf.sprintf
        {|%s**************************************************************************%s
%s*        ^o3                                                             *%s
%s* ~/\_/\_|)                       OCaml                                  *%s
%s* |/=_=\|                                                                *%s
%s* "     "                                                                *%s
|}
        comment_open comment_close comment_open comment_close comment_open
        comment_close comment_open comment_close comment_open comment_close )
    str

let replace_header_file filename =
  let file = In_channel.open_text filename in
  let str = In_channel.input_all file in
  In_channel.close file ;
  let str' = replace_header str in
  if str <> str' then (
    let file = Out_channel.open_text filename in
    output_string file str' ; close_out file )

let main args =
  List.iter
    (fun file -> try Printexc.print replace_header_file file with _ -> ())
    args

open Cmdliner

let cmd =
  Cmd.(v (info "ascii_camel"))
    Term.(const main $ Arg.(value (pos_all file [] (info []))))

let () = exit @@ Cmd.eval cmd
