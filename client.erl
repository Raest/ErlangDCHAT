-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").
-import(lists, [member/2]).


%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
% Takes a server and machine as an input which it then tries to connect to. Since Shire was the specification it is hardcoded. Other ways???
% Interperts the results from server and gives the apropriate errors.
loop(St, {connect, {_Server, _Machine}}) ->
    Con = (catch(genserver:request({list_to_atom(_Server), list_to_atom(_Machine)}, {connect, St#cl_st.nick}))),
	case Con of 
		{'EXIT',_} -> {{error, server_not_reached, "Server gone, come back later"}, St};
		not_ok -> {{error, user_already_connected, "User already connected"}, St};
		ok -> St2 = St#cl_st{server = {list_to_atom(_Server), list_to_atom(_Machine)}}, {ok, St2}
	end;

% Takes a server as an input which it then tries to connect to. Since Shire was the specification it is hardcoded. Other ways???
% Interperts the results from server and gives the apropriate errors.
loop(St, {connect, _Server}) ->
    Con = (catch(genserver:request(list_to_atom(_Server), {connect, St#cl_st.nick}))),
	case Con of 
		{'EXIT',_} -> {{error, server_not_reached, "Server gone, come back later"}, St};
		not_ok -> {{error, user_already_connected, "User already connected"}, St};
		ok -> St2 = St#cl_st{server = list_to_atom(_Server)}, {ok, St2}
	end;		

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
% Takes no input and similarly to connect the server shire is hardcoded. Appropriate respones are given to the different errors.
% The corectness test says there are things wrong here. Probably because of the hardcoding. Not sure how to fix this?
loop(St, disconnect) ->
	Server = St#cl_st.server,
	case Server of
	"" -> {{error, user_not_connected, "YOU ARE NOT EVEN CONNECTED"}, St};
	_ -> 
		case length(St#cl_st.channels) of
		0 ->
			Dis = (catch(genserver:request(St#cl_st.server, {disconnect, St#cl_st.nick}))),
			case Dis of 
				{'EXIT',_} -> {{error, server_not_reached, "Something went VERYVERY wrong"}, St};
				not_ok -> {{error, user_not_connected, "YOU ARE NOT EVEN CONNECTED"}, St};
				ok -> St2 = St#cl_st{server = ""}, {ok, St2}
			end;
		_ -> {{error, leave_channels_first, "Leave your channels though guy!"}, St}
		end
	end;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
% Takes a channel name as an input and tries to join the channel. The server will in turn say if it works or not.
% errors are then given as need be. If the client is sucsesful in joining the channel the client will keep track of the entry.
loop(St,{join,_Channel}) ->
	Server = St#cl_st.server,
	case Server of
	"" -> {{error, user_not_connected, "YOU ARE NOT EVEN CONNECTED"}, St};
	_ -> 
		Join = genserver:request(St#cl_st.server, {join, _Channel, self()}),
		case Join of 
			error -> {{error, user_already_joined, "User already connected to channel"}, St};
			ok -> St2 = St#cl_st{channels = St#cl_st.channels++[_Channel]}, {ok, St2}
		end
	end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
% Takes a channel name as an input and tries to leave the channel. The server will in turn say if it works or not.
% errors are then given as need be. If the client is sucsesful in leaving the channel the client will be uppdated with a new channellist.
loop(St, {leave, _Channel}) ->
 	Server = St#cl_st.server,
	case Server of
	"" -> {{error, user_not_connected, "YOU ARE NOT EVEN CONNECTED"}, St};
	_ -> 
		Leave = genserver:request(St#cl_st.server, {leave, _Channel, self()}),
		case Leave of 
			error -> {{error, user_not_joined, "User is not a member of this channel"}, St};
			ok -> St2 = St#cl_st{channels = St#cl_st.channels--[_Channel]}, {ok, St2}
		end
	end;    

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
% Takes a channel and a message as input and sends it. If the server is unsucessful it will return appropriate errors.
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
     case member(_Channel, St#cl_st.channels) of
     	true ->	
    	Message = genserver:request(St#cl_st.server,{msg_from_GUI,_Channel,_Msg,St#cl_st.nick,self()}),
     	case Message of
     		ok -> {ok, St};
     		error -> {{error, user_not_joined, "Channel does not exist"}, St}
    	end;
    	false -> {{error, user_not_joined, "Channel does not exist"}, St}
	 end;




%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
% Returns the name of the user which is stored in the client process
loop(St, whoiam) ->
	{St#cl_st.nick, St};

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
% Changes the the name of the client process
loop(St,{nick,_Nick}) ->
    {ok, St#cl_st{ nick = _Nick }};

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St};

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
% Recieves strings of channel, nick and message which is sent to the GUI.
loop(St = #cl_st { gui = GUIName }, {_Chan, _Nick, _Msg}) ->
    %{Channel, Name, Msg} = decompose_msg(_MsgFromClient),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, _Chan, _Nick++"> "++_Msg}),
    {ok, St}.

%length([]) -> 0;
%length([H|T]) -> 1 + length([T]).

% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
%decompose_msg(_MsgFromClient) ->
%    {"", "", ""}.


%%%%%%%%%%%%%%%%%%%%%
%%%% Initial state
%%%%%%%%%%%%%%%%%%%%%
% Initial state with inital variables set as empty and will be assigned as they are needed/used.
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, server = "", channels=[]}.
