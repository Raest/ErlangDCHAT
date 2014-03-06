% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
% nick: it stores the client name.
% server: it stores which server the client is connected to.
% channels: it stores which channels the client is in.
%
-record(cl_st, {gui,nick, server,channels}).
    
% This record defines the structure of the 
% server process. 
% 
% It containts the following fields:
%
% server: it stores the server name.
% users: it stores the users that are connected to the server.
% channels: it stores what channels exist.
%
-record(server_st, {server, users, channels}).



% This record defines the structure of the 
% channel process. 
% 
% It containts the following fields:
%
% channel: it stores the name of the channel.
% userpids: it stores the user proccesses that are connected to the channel
%
-record(channel_st, {channel, userpids}).

