-module(bel_scan_eel_eng).
-behaviour(bel_scan_eng).

% bel_scan_eng callbacks
-export([ init/1
        , handle_start/2
        , handle_text/2
        , handle_match/2
        , handle_terminate/2
        ]).

-include("bel_scan_eng.hrl").

init(_Opts) ->
    #engine{
        markers = [
            #marker{
                id = inline,
                re = <<
                    "(<%=\\s+)(.*)?(\\s+\\.%>)" "|"
                    "(<%=\\s+)(.*)?(^(\\s*\\.%>))"
                >>
            },
            #marker{
                id = start,
                re = <<
                    "(<%=\\s+)(.*)?(\\s+%>)" "|"
                    "(<%=\\s+)(.*)?(^(\\s*%>))"
                >>
            }
            #marker{
                id = continue,
                re = <<
                    "(<%\\s+)(.*)?(\\s+%>)" "|"
                    "(<%\\s+)(.*)?(^(\\s*%>))"
                >>
            },
            #marker{
                id = terminate,
                re = <<
                    "(<%\\s+)(.*)?(\\s+\\.%>)" "|"
                    "(<%\\s+)(.*)?(^(\\s*\\.%>))"
                >>
            },
            #marker{
                id = comment,
                re = <<
                    "(<%!--\\s+)(.*)?(\\s+--%>)" "|"
                    "(<%!--\\s+)(.*)?(^(\\s*--%>))"
                >>
            }
        ]
    }.

handle_start(_Bin, State) ->
    {noreply, State}.

handle_text(_Text, State) ->
    {noreply, State}.

handle_match({?MODULE, MarkerId, Captured}, State) ->
    [_StartMarker, Expr, _EndMarker] = Captured,
    Token = bel_scan:token(MarkerId, Expr, State),
    {reply, [Token], State};
handle_match(_Match, State) ->
    {noreply, State}.

handle_terminate(_Tokens, State) ->
    {noreply, State}.
