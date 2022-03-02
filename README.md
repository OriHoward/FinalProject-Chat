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
2) Enter the IP of the server - to find the IP of the server type ipconfig in the pc the server is running from and take
   the IPv4 Address.  
   If in localhost type: localhost or leave the field empty.

#### Chat window:

In the Chat window you have 3 main buttons:

1) Disconnect - to disconnect from chat
2) Download File - click this button to download files from the server. the button will open a window for you to type
   the file name you wish to download.
3) Show commands - click this button to show all the possible commands for the client to use.

#### Commands:

1) /users - shows the client all the connected users in the chat
2) /files - shows the client all the files he can download from the server.
3) /whisper <client_name> <msg - to send a private message to another client
4) /disconnect - to disconnect from chat

HAVE FUN !

## About the project

In this assigment we were asked to build a chat that has server side and client side.  
The server takes care of all the clients requests, and the clients can communicate with each other through the server.  
Every message sent in Chat is going through the server which knows how to distribute the message to all the connected
users.  
In addition, we were asked to add an option for the client to download a file through a UDP socket.  
The UDP socket needs to be reliable and FAST. That is why I choose to implement the Selective Repeat Algorithm.

### How Does it work?

The server is listening on a specific port known in advance to all clients.  
Each client who wants to connect needs to know the IP and port of the server. The client and the server are connected
through a TCP socket.  
After the server accepts the client request to connect, it opens a thread for it which is responsible to listen in a
loop for all the requests from the client and take care each one of them. Each client in the Chat can do multiple
things: sending messages in the chat for everyone,sending private messages to another client, ask the server to get the
file list and even download the file itself. To download a file the clients open another socket with the server on UDP
which will be explained in details below.

In general: All clients can communicate with each other in the Chat while the server taking care of all the messages
that are sent.

## The main Algorithm

The main algorithm of the assigment is the selective repeat algorithm. When client is asking to download a file we open
UDP socket for the client and the server to connect.  
The UDP "doesn't care" if a packet is lost which also makes it is much faster.  
The goal of the assigment is to take care of the lost packets that going through the UDP socket.  
There are a lot of ways and algorithms to do so like: Stop and Wait,Go back N and Selective repeat. I chose to use
selective repeat because it is one of the fastest.

### The full process of the file transfer:

The UDP socket cannot transfer more than 64kb that is why we had to read the file part by part and make from each data
part a packet.  
The packet I built are made of Sequence number for each packet and the data itself and each packet built is inserted
into a list that holds all the packets.  
After we have a list of packets we need to transfer all those packets to the client and then the client will take apart
each packet and save the data. The data then will be written back to a file in the right order. But how do we send those
packets without losing them in the process? That is where the Selective repeat Algorithm comes in hand.  
How does the Selective repeat works? We start with a window size of 4, so at the start we send the first 4 packets. For
each packet the client receives he sends ACK back to Server with the sequence number of the packet which means he
received it successfully. For every ACK we receive from the client we check if the ACK we received is the one we were
expecting to get. If it is, we know a packet has arrived, and we can send another one. If it isn't we check which
packets were lost on the way and resend them. Everytime we receive a correct ACK we increase the size of the window to
send more packets, but if we get the wrong packet back, or we get a timeout we decrease the size of the window
accordingly and resend those packets who haven't arrived. On the client side the client is just receiving those packets
and sending back ACK accordingly.  
This algorithm makes sure no packets are lost on the way, and it is efficient and fast because we resend only the
packets who haven't arrived.

# Client side

The main class of the client side is the Client.py.  
It has multiple functions that are necessary for each client to communicate with the server

### The client has the following classes as well:

1) Actions - has special enums so that the client and the server will have a common language.
2) ButtonActions - responsible for the GUI buttons action.
3) ChatGUI - the Graphic User Interface class.
4) HandleThreads - handles the threads in the client side
5) SocketHandler - this class responsible to send messages from the server and get messages from the server.
6) RunClient - main to run the client
7) RunMultipleClients - main to run multiple clients in a local network

# Server side
The main class is the Server.py  
The server has all the main functions he needs to take care for all clients.
Those functions are explained in detail in the code itself

### The server has the following classes as well:
1) Actions - has special enums so that the server and the server client have a common language.
2) SocketHandler - this class responsible to send messages from the server and get messages from the server.
3) Client - The server saves all the information (connection for example) he has with each client in this class.
4) HandleClients - responsible to handle with all the clients requests.
5) RunServer - main to run the server
