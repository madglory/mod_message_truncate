-module(mod_message_truncate).

-behaviour(gen_mod).

-include("logger.hrl").
-include("xmpp.hrl").

-export([
  start/2,
  stop/1,
  on_filter_packet/1,
  mod_opt_type/1,
  depends/2
]).

-include("ejabberd.hrl").

filterMessageText(MessageText) ->
  Length = length(MessageText),
  MaxMessageLength = 20,
  if
    length(MessageText) > MaxMessageLength ->
      lists:concat([lists:sublist(MessageText, MaxMessageLength - 3), "..."]);
    true ->
      MessageText
  end.

start(_Host, _Opts) ->
  ?INFO_MSG("Starting mod_message_truncate", [] ),
  ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 1),
  ok.

stop(_Host) ->
?INFO_MSG("Stopping mod_message_truncate", [] ),
  ejabberd_hooks:delete(filter_packet, global, ?MODULE, on_filter_packet, 1),
  ok.


on_filter_packet(Msg) ->
  Type = xmpp:get_type(Msg),
  if 
    (Type == chat) orelse (Type == groupchat)  ->
      BodyText = xmpp:get_text(Msg#message.body),
      if
        (BodyText /= <<>>) ->
          NewBody = binary:list_to_bin(filterMessageText(binary:bin_to_list(BodyText))),
          [BodyObject|_] = Msg#message.body,
          NewBodyObject = setelement(3, BodyObject, NewBody),
          NewMsg = Msg#message{body = [NewBodyObject]},
          NewMsg;
        true ->
          Msg
      end;
    true -> 
      Msg
  end.



mod_opt_type(_) -> [].
depends(_Host, _Opts) -> [].