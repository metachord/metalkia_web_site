% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tagsinput).
-include_lib("nitrogen_core/include/wf.hrl").
-include("mt_elements.hrl").
-compile(export_all).

reflect() -> record_info(fields, tagsinput).

render_element(Record) ->
    ID = Record#tagsinput.id,
    Anchor = Record#tagsinput.anchor,
    case Record#tagsinput.next of
        undefined -> ignore;
        Next ->
            Next1 = wf_render_actions:normalize_path(Next),
            wf:wire(Anchor, #event { type=enterkey, actions=wf:f("Nitrogen.$go_next('~s');", [Next1]) })
    end,

    case Record#tagsinput.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=enterkey, postback=Postback, validation_group=ID, delegate=Record#tagsinput.delegate })
    end,

    Value = wf:html_encode(Record#tagsinput.text, Record#tagsinput.html_encode),
    RawID = case ID of [$. | RestId] -> RestId; _ -> ID end,
  [
    wf_tags:emit_tag(input, [
      {id, RawID},
      {class, [tagsinput, Record#tagsinput.class]},
      {maxlength, Record#tagsinput.maxlength},
      {style, Record#tagsinput.style},
      {value, Value}
    ]),
    "<script type=\"text/javascript\">
    $('#" ++ RawID ++ "').tagsInput({height:'30px', inputPadding: '0px', autoFill:'true'});
    </script>
    "
  ].

