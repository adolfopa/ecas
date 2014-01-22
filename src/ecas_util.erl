-module(ecas_util).
-author("Adolfo Perez Alvarez <adolfo.pa@gmail.com>").

-export([is_get/1, 
         is_post/1, 
         parse_post_params/1, 
         get_param/2, 
         set_cookie/4, 
         render/3,
         render/2, 
         render/1, 
         redirect/3, 
         add_q/3]).

is_get(ReqData) ->
    method_eq(ReqData, 'GET').

is_post(ReqData) ->
    method_eq(ReqData, 'POST').

method_eq(ReqData, Method) ->
    wrq:method(ReqData) =:= Method.

parse_post_params(ReqData) ->
    mochiweb_util:parse_qs(wrq:req_body(ReqData)).

get_param(Name, Params) ->
    proplists:get_value(Name, Params, "").

set_cookie(ReqData, Name, Value, Options) ->
    {HName, HValue} = mochiweb_cookies:cookie(Name, Value, Options),
    wrq:set_resp_header(HName, HValue, ReqData).

add_q(URL, Name, Value) ->
    URI = uri:from_string(uri:unquote(list_to_binary(URL))),
    binary_to_list(uri:to_string(uri:q(URI, [{Name, Value}]))).

redirect(URL, ReqData, State) ->
    {{halt, 303}, wrq:set_resp_header("Location", URL, ReqData), State}.

render(Template, Env) when is_list(Env) ->
    {ok, Content} = Template:render(Env),
    Content;
render(Template, ReqData) ->
    render(Template, [], ReqData).

render(Template) ->
    render(Template, []).

render(Template, Env, ReqData) ->
    Content = render(Template, Env),
    wrq:append_to_response_body(Content, ReqData).
