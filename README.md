# FinalProject-Chat
### code written by Ori Howard
## Requirements:
Windows 10 - pip install tk  
Linux - apt-get install python-tk  
Python version 3.10


## How to Run the Code:
First run the server side: 
```shell
python3 RunServer.py
```
Then run the client side:
```shell
python3 RunClient.py
```
Test multiple clients on local network: 
```shell
python3 RunMultipleClients.py
```
## How to use the Graphic User Interface:
#### Login window:
1) Enter Username - one word only characters
2) Enter the IP of the server - to find the IP of the server type ipconfig in the pc the server is running from and take the IPv4 Address.  
If in localhost type: localhost or leave the field empty.

#### Chat window:
In the Chat window you have 3 main buttons:
1) Disconnect - to disconnect from chat
2) Download File - click this button to download files from the server. the button will open a window for you to type the file name you wish to download.
3) Show commands - click this button to show all the possible commands for the client to use.  
#### Commands:
1) /users - shows the client all the connected users in the chat
2) /files - shows the client all the files he can download from the server.  
3) /whisper <client_name> <msg - to send a private message to another client
4) /disconnect - to disconnect from chat

HAVE FUN !

## About the project
In this assigment we were asked to build a chat that has server side and client side.


# Client side
## The main class:

### The client has the following classes:
1) Actions - has special enums so that the client and the server will have a common language.
2) ButtonActions - responsible for the GUI buttons action.
3) ChatGUI - the Graphic User Interface class.
4) HandleThreads - handles the threads in the client side
5) SocketHandler - this class responsible to send messages from the server and get messages from the server.
6) Client - the main class which represents the client. the class has many functions to help the client communicate with the server.




# Server side
### The server has the following classes:
1) Actions - has special enums so that the server and the server client have a common language.
2) SocketHandler - this class responsible to send messages from the server and get messages from the server.
3) Server - the main class which represents the server. 


# The communication:
