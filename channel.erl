-module(channel).
-export([loop/2, initial_state/1]).
-import(lists, [member/2]).
-import(erl, [display/1, foreach/2, to_atom/1]).
-include_lib("./defs.hrl").

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
% Takes a Pid as an input and checks if it is already in a channel or not.
% If it isn't in the channel it will be added to the channel otherwise an error is returned.
loop(St,{join, _Pid}) ->
	case member(_Pid, St#channel_st.userpids) of 
		true 	->	{error, St};
		false	->	St2 = St#channel_st{userpids = St#channel_st.userpids++[_Pid]}, 
					{ok,St2} 
	end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
% Takes a Pid as an input and checks if it is in the channel or not.
% Returns an error if it wasn't in the channel in the first place, otherwise it is removed from the channellist.
loop(St,{leave, _Pid})->
	case member(_Pid, St#channel_st.userpids) of 
		true	->	St2 = St#channel_st{userpids = St#channel_st.userpids--[_Pid]}, 
					{ok,St2};
		false 	->	{error, St} 
	end;

%%%%%%%%%%%%%%%
%%%% Messages
%%%%%%%%%%%%%%%
% Takes a message, nick and Pid  as an input to then go through all other Pids in the channel and send messages to them.
loop(St,{_Msg, _Nick, _Pid}) ->
	Chan = St#channel_st.channel,
	%List with Pids that should recieve the message.
	List = [ Pid || Pid <- St#channel_st.userpids, Pid =/= _Pid], 
	iterate(List, {Chan, _Nick, _Msg}),
	{ok, St}.

% Iterator that is used to start client processes so they send messages to their respective GUIs.	
iterate([], _Msg) -> ok;
iterate([H|T], _Msg) ->
	spawn(fun() -> send(H,_Msg) end),
	iterate(T, _Msg).

send(_Pid, _Msg) ->
	genserver:request(_Pid, _Msg).

%%%%%%%%%%%%%%%%%%%%%
%%%% Initial state
%%%%%%%%%%%%%%%%%%%%%
% Initial state with inital variables set as empty and will be assigned as they are needed/used.
initial_state(_Channel) ->
    #channel_st{channel = _Channel, userpids = []}.