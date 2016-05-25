#Erlang implementation of Bluff CLI card game 

**Rules**

Rules ”Bluff” is a popular card game which is played with a standard 52 card trump and can be played by from 2 to 10 players. All the cards are dealt out to the players; some may have more than others, but not by much. The object is to get rid of all your cards. Select at random who should go first and continue clockwise. On the table is a discard pile, which starts empty. A turn consists of discarding one or more cards face down on the pile, and calling out their rank. The first player must discard Aces, the second player discards Twos, the next player Threes, and so on. After Tens come Jacks, then Queens, then Kings, then back to Aces, etc. Since the cards are discarded face down, you do not in fact have to play the rank you are calling. For example if it is your turn to discard Sevens, you may actually discard any card or mixture of cards; in particular, if you don’t have any Sevens you will be forced to play some other card or cards. Any player who suspects that the card(s) discarded by a player do not match the rank called can challenge the play by calling ”Bluff!”. Then the cards played by the challenged player are exposed and one of two things happens:
- if they are all of the rank that was called, the challenge is false, and the challenger must pick up the whole discard pile;
- if any of the played cards is different from the called rank, the challenge is correct, and the person who played the cards must pick up the whole discard pile.

After the challenge is resolved, play continues in normal rotation: the player to the left of the one who was challenged plays and calls the next rank in sequence. The first player to get rid of all their cards and survive any challenge resulting from their final play wins the game. If you play your last remaining card(s), but someone challenges you and the cards you played are not what you called, you pick up the pile and play continues.

**Allowed variation based on location and habits**

Instead of playing turn after turn with increasing rank, in this implementation every player can throw any of his cards and declare whatever seed he wants, i.e. is more of a random thing based on the trust each player has about his friends and their reaction to the game. :)

**How to play**
- In terminal, ```cd``` to this game's directory
- Open as many shells as the number of player + 1 (server)
- In the first shell (server of the game) type: 
    - ```erl -sname server@bluff -setcookie {password}```
    - ```(server@bluff)1> c(server).```
    - ```server:start(NumberOfPlayers).``` 
        - ```NumberOfPlayers = integer()```
        - i.e. ```(server@bluff)2> server:start(3).```
    - Server now outputs game's flow
- In the other shells type:
    - ```erl -sname {name of player} -setcookie {password} -remsh server@bluff```
    - ```game:enter(Name).```
        - ```Name = string()```
        - i.e. ```(server@bluff)1> game:enter("Romeo").```
    - The above command also shows the player his cards
    - Once everybody is in the game, everybody need to be ready in order for the game to really start. This means that everybody has to type ```(server@bluff)2> game:ready().```
    - The player chosen from the server can now play his cards by typing ```game:play(CardsIndex, DeclaredRank).```
        - ```CardsIndex = List = [term()]```
        - ```DeclaredRank = string()```
        - i.e. ```(server@bluff)3> game:play([1,2,5], "Diamonds").```
    - The server outputs the next player but everybody can type ```(server@bluff)4> game:bluff().``` to accuse last player of bluffing
    - The server handles this and based on the cards the last player played, it gives back the full game deck (which is saved in the server) to the previous player or the one who is declaring the bluff
    - The game continues until a player who played his last cards is **erroneously** accused of **bluffing**
