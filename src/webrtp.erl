-module(webrtp).

-export([get_page/1, get_error_page/1, post/2, parse_packet/1]).

-include_lib("../nksip/include/nksip.hrl").

-compile([{parse_transform, lager_transform}]).

-define(DOMAIN, "ltx").

%%% Return priv/www/index.html page to the HTTP GET request 
get_page(Status) ->
  {ok, Head} = file:read_file("priv/www/index.html"),
  Outback = case Status of
              init -> "READY TO SPEAK";
              {post, Phone} ->
                "SUCCESS. User <"++binary_to_list(Phone)++"> heard your message"
  end,
  Tail = "<p>Status: "++Outback++"</p>",
  Sep = "\r\n    ",
  TailPage = ["</form>", Tail,"</body>", "</html>"],
  Add=list_to_binary(string:join(TailPage, Sep)),
  Binary = <<Head/binary, Add/binary>>,
  Size = erlang:byte_size(Binary),
  BinSize = erlang:integer_to_binary(Size),
  HTTP = <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\n">>,
  <<HTTP/binary, Binary/binary>>.


%%% Return page with the reason of the error to the Web
get_error_page({Class, Reason, Stacktrace}) ->
  lager:error("~nStacktrace:~s", [lager:pr_stacktrace(Stacktrace, {Class, Reason})]),
  {ok, Binary} = file:read_file("priv/www/error.html"),
  Size = erlang:byte_size(Binary),
  BinSize = erlang:integer_to_binary(Size),
  HTTP = <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\n">>,
  <<HTTP/binary, Binary/binary>>.

%%% Processes the HTTP POST request by the specified Phone&Text and forms the answer
post(Phone, Text) ->
  case sip_invite(unicode:characters_to_list(Phone), unicode:characters_to_list(Text)) of
    {ok, success} ->
      get_page({post, Phone});
    {C, R, S} ->
      get_error_page({C, R, S})
  end.

%%% Parsing msg from client
parse_packet(Packet) ->
  case string:split(Packet, "/") of
    [<<"POST ">> | _T] ->
      case string:split(Packet, "\r\n\r\n") of
        [_ | Post] when Post =/= [] ->
          [PhonePart | TextPart] = string:split(Post, "&"),
          [_ | Phone] = string:split(PhonePart, "phone="),
          [_ | Text] = string:split(erlang:hd(TextPart), "text="),
          {ok, post, erlang:hd(Phone), erlang:hd(Text)};
        _ -> {error, badrequest, erlang:get_stacktrace()}
      end;
    [<<"GET ">> | _T] -> {ok, get, Packet};
    _ -> {error, badrequest, erlang:get_stacktrace()}
  end.

%%% Just trying make the call
sip_invite(Phone, Text) ->
  try make_call(Phone, Text) of
    ok ->
      {ok, success}
  catch
    Class:Reason ->
      {Class, Reason, erlang:get_stacktrace()}
  end.

%%% Launches SIP client, registers and does invite
make_call(Phone, Text) ->
  {ok, PBX_IP} = application:get_env(webrtp, pbx_ip),
  Client1 = string:concat("sip:1000@", ?DOMAIN),
  StartOptions = #{sip_from => Client1,
    plugins => [nksip_uac_auto_auth],
    sip_listen => "<sip:all:10000>, <sip:all:10001;transport=udp>"},

  case nksip:start_link(client1, StartOptions) of
    {ok, _} -> ok;
    {error, Term} ->
      erlang:error(Term)
  end,

  PBX_Addr = string:concat("sip:", PBX_IP),
  RegOptions = [{sip_pass, "12345"}, contact, {meta, ["contact"]}],

  case nksip_uac:register(client1, PBX_Addr, RegOptions) of
    {ok, 200, _} -> ok;
    Error ->
      lager:warning("Register problem: ", [Error])
  end,

  Client2 = "sip:" ++ Phone ++ "@" ++ PBX_IP,

  SDP = #sdp{address = {<<"IN">>, <<"IP4">>, erlang:list_to_binary(PBX_IP)},
    connect = {<<"IN">>, <<"IP4">>, erlang:list_to_binary(PBX_IP)},
    time = [{0, 0, []}],
    medias = [#sdp_m{media = <<"audio">>,
      port = 9990,
      proto = <<"RTP/AVP">>,
      fmt = [<<"0">>, <<"101">>],
      attributes = [{<<"sendrecv">>, []}]
    }
    ]
  },

  InviteOptions = [{add, "x-nk-op", ok},
    {add, "x-nk-prov", true},
    {add, "x-nk-sleep", 10000},
    auto_2xx_ack,
    {sip_dialog_timeout, 10000},
    {sip_pass, "12345"},
    {body, SDP}
  ],

  invite(3, Client2, InviteOptions, Text),    % insofar as timeout didn't work trying to invite 3 times
  nksip:stop(client1).

%%% Is trying make invite until Acc > 0
invite(0, _, _, _) ->
  erlang:error(noinvite);
invite(Acc, Client2, InviteOps, Text) when Acc > 0 ->
  case nksip_uac:invite(client1, Client2, InviteOps) of
    {ok, 200, [{dialog, DlgId}]} ->
      {ok, SDPRemoteVoice} = nksip_dialog:get_meta(invite_remote_sdp, DlgId),
      [SDP_M | _] = SDPRemoteVoice#sdp.medias,
      Port = SDP_M#sdp_m.port,
      {ok, PBX_IP} = application:get_env(webrtp, pbx_ip),
      GenVoice = "wget -O priv/voice/generate.wav \"https://tts.voicetech.yandex.net/generate?format=wav&lang=ru_RU&key=069b6659-984b-4c5f-880e-aaedcfd84102&text="
        ++ Text ++ "\"",
      ConvertVoice = "ffmpeg -i priv/voice/generate.wav -codec:a pcm_mulaw -ar 8000 -ac 1 priv/voice/output.wav -y",
      StartVoice = "./voice_client priv/voice/output.wav " ++ PBX_IP ++ " " ++ erlang:integer_to_list(Port),
      Cmd = GenVoice ++ " && " ++ ConvertVoice ++ " && " ++ StartVoice,
      Res = os:cmd(Cmd),
      ResBin = unicode:characters_to_binary(Res),
      lager:info("Result cmd: ~s", [ResBin]),
      nksip_uac:bye(DlgId, []),
      ok;
    _Error ->
      timer:sleep(4000),
      invite(Acc - 1, Client2, InviteOps, Text)
  end.
