-ifndef(_MT_ELEMENTS_H).
-define(_MT_ELEMENTS_H, true).

-include_lib("nitrogen_core/include/wf.hrl").

-record(tagsinput, {?ELEMENT_BASE(element_tagsinput), text="", maxlength="", html_encode=true, next, postback, delegate}).

-endif. %% _MT_ELEMENTS_H
