-module(mod_message_truncate).

-behaviour(gen_mod).

-include("logger.hrl").

-export([
  start/2,
  stop/1,
  on_filter_packet/1,
  mod_opt_type/1,
  depends/2
]).

-include("ejabberd.hrl").

filterMessageText(MessageText) ->
  MaxMessageLength = 1000,
  if
    length(MessageText) > MaxMessageLength ->
      lists:concat([lists:sublist(MessageText, MaxMessageLength - 3), "..."]);
    true ->
      MessageText
  end.

filterMessageBodyElements([{xmlel, <<"body">>, BodyAttr, [{xmlcdata, MessageText}]} = _H|T], MessageElements) ->
  FilteredMessageText = filterMessageText(binary:bin_to_list(MessageText)),
  FilteredBody = {xmlel, <<"body">>, BodyAttr, [{xmlcdata, FilteredMessageText}]},
  filterMessageBodyElements(T, lists:append(MessageElements, [FilteredBody]));

filterMessageBodyElements([H|T], MessageElements) ->
  % skip this tag, but pass it on as processed
  filterMessageBodyElements(T, lists:append(MessageElements, [H]));

filterMessageBodyElements([], MessageElements) ->
  MessageElements.

start(_Host, _Opts) ->
  ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 0),
  ok.

stop(_Host) ->
  ejabberd_hooks:delete(filter_packet, global, ?MODULE, on_filter_packet, 0),
  ok.

on_filter_packet(drop) ->
  drop;

on_filter_packet({_From, _To, {xmlel, <<"message">>, _Attrs, Els} = _Packet} = _Msg) ->
  FilteredEls = filterMessageBodyElements(Els, []),
  {_From, _To, {xmlel, <<"message">>, _Attrs, FilteredEls}};
on_filter_packet(Msg) ->
  % Handle the generic case (any packet that isn't a message with a body).
  Msg.

mod_opt_type(_) -> [].
depends(_Host, _Opts) -> [].
