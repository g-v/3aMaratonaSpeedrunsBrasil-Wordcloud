

import re
import socket
import random
import sys
import time
from datetime import datetime

# --------------------------------------------- Start Settings ----------------------------------------------------
HOST = "irc.twitch.tv"                          # Hostname of the IRC-Server in this case twitch's
PORT = 6667                                     # Default IRC-Port
CHAN = "#"                                      # Channelname = #{Nickname}
NICK = ""                                       # Nickname = Twitch username
PASS = "oauth:authkey"                          # www.twitchapps.com/tmi/ will help to retrieve the required authkey
# --------------------------------------------- End Settings -------------------------------------------------------

authorized = {''}

random.seed()



# --------------------------------------------- Start Functions ----------------------------------------------------
def send_pong(msg):
    con.send(bytes('PONG %s\r\n' % msg, 'UTF-8'))


def send_message(chan, msg):
    con.send(bytes('PRIVMSG %s :%s\r\n' % (chan, msg), 'UTF-8'))


def send_nick(nick):
    con.send(bytes('NICK %s\r\n' % nick, 'UTF-8'))


def send_pass(password):
    con.send(bytes('PASS %s\r\n' % password, 'UTF-8'))


def join_channel(chan):
    con.send(bytes('JOIN %s\r\n' % chan, 'UTF-8'))


def part_channel(chan):
    con.send(bytes('PART %s\r\n' % chan, 'UTF-8'))
# --------------------------------------------- End Functions ------------------------------------------------------


# --------------------------------------------- Start Helper Functions ---------------------------------------------
def get_sender(msg):
    result = ""
    for char in msg:
        if char == "!":
            break
        if char != ":":
            result += char
    return result


def get_message(msg):
    result = ""
    i = 3
    length = len(msg)
    while i < length:
        result += msg[i] + " "
        i += 1
    result = result.lstrip(':')
    return result

words = []
t1 = datetime.now()
t2 = None
deltaT = None


def parse_message(msg, sender):
	global t1
	global t2
	global deltaT
	
	t2 = datetime.now()
	if len(msg) >= 1:
		
		dontWrite = False
		global words
		
		if sender in authorized: #command to write
			msgNova = msg.split(' ')
			if msgNova[0] in {'!write'}:
				dontWrite = True
				writeToFile(words)
				t1 = datetime.now()
				words[:] = []
		
		if dontWrite is False:
			words.append(msg)
		
		deltaT = t2 - t1
		if deltaT.total_seconds() > 3599: #minimum time to write to text file 
			writeToFile(words)
			t1 = datetime.now()
			words[:] = []
		


def writeToFile(w):
	with open('textoMaratona.txt', 'a', encoding='utf8') as file:
		file.write('\n')
		file.write('\n'.join(w))
	#erase list, text is written in file
	
# --------------------------------------------- End Helper Functions -----------------------------------------------


con = socket.socket()
con.connect((HOST, PORT))

send_pass(PASS)
send_nick(NICK)
join_channel(CHAN)
data = ""



print("Bot está conectado em " + CHAN)


while True:
	try:
		data = data+con.recv(1024).decode('UTF-8')
		data_split = re.split(r"[~\r\n]+", data)
		data = data_split.pop()
		
		for line in data_split:
			line = str.rstrip(line)
			line = str.split(line)
            
			if len(line) >= 1:
				if line[0] == 'PING':
					send_pong(line[1])
				
				try:
					if line != None:
						try:
							if line[1] == 'PRIVMSG':
								sender = get_sender(line[0])
								message = get_message(line)
								a = parse_message(message, sender)
								if a == -1:
									sys.exit(0)
								#print(sender + ": " + message)
						except:
							print("erro depurado :D")
				except:
					print("erro depurado :D")
					
	except socket.error:
		print("Socket died")

	except socket.timeout:
		print("Socket timeout")
    