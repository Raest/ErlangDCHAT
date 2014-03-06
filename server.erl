-module(server).
-export([loop/2, initial_state/1]).
-import(lists, [member/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
% Checks if Nick is already connected and returns result to client process.
% If nick is not connected it uppdates the server state with the new user.
loop(St, {connect, _Nick}) -> 
	case member(_Nick, St#server_st.users) of
		true ->  {not_ok, St};
		false -> St2 = St#server_st{users = St#server_st.users++[_Nick]},
    	{ok, St2}
	end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
% Checks if Nick is already connected and returns result to client process.
% If nick is connected it uppdates the server state without the new user.
loop(St, {disconnect, _Nick}) -> 
	case member(_Nick, St#server_st.users) of
	false -> {not_ok, St};
	true ->  St2 = St#server_st{users = St#server_st.users--[_Nick]},
    {ok, St2}
	end;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
% Takes a channel and Pid as input to then see if channel exisits or not.
% If the channel doesn't exisits it creates it and then join otherwise it just joins.
loop(St, {join, _Channel, _Pid}) ->
	case member(_Channel, St#server_st.channels) of
		true -> 
			Join = genserver:request(list_to_atom(_Channel), {join, _Pid}),
			case Join of
				error 	-> {error, St};
				ok 		-> {ok,St}
		end;
		false -> 
			genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel), 
                    fun channel:loop/2),
			St2 = St#server_st{channels = St#server_st.channels++[_Channel]},
			genserver:request(list_to_atom(_Channel), {join, _Pid}),
			{ok, St2}
		end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
% Takes a channel and Pid as input to then see if channel exisits or not.
% If the channel exisits it tells the channel to remove the client, otherwise it sends an error.
loop(St, {leave, _Channel, _Pid}) ->
	case member(_Channel, St#server_st.channels) of
		true -> 
		Leave = genserver:request(list_to_atom(_Channel), {leave, _Pid}),
		case Leave of
			ok -> {ok,St};
			error -> {error, St}
		end;
		false -> {error, St} 
	end;	

%%%%%%%%%%%%%%%
%%%% Messages
%%%%%%%%%%%%%%%
% Passes along the request from the client to the channel of message sending.
loop(St, {msg_from_GUI, _Channel, _Msg, _Nick, _Pid}) -> 
	case member(_Channel, St#server_st.channels) of
		true ->	genserver:request(list_to_atom(_Channel),{_Msg, _Nick, _Pid}),
		{ok,St};
		false -> {error, St} 
	end.	

%%%%%%%%%%%%%%%%%%%%%
%%%% Initial state
%%%%%%%%%%%%%%%%%%%%%
% Initial state with inital variables set as empty and will be assigned as they are needed/used.
initial_state(_Server) ->
    #server_st{server = _Server, users = [], channels = []}.