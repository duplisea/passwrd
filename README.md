# passwrd
Multiple password generator

This is not secure in the sense of encryption secture and it has lots of limitations. In fact there is nothing
encrypted. It just generates a long random number sequence based on how you start it out. If you always start it
the same way, it will always generate the same sequence.

Because random sequences generated by a computer are not actually random and can be perfectly duplicated by
setting the seed to the same value each time, you can re-generate a password by using a master password to set
the seed. So essentially this code generates a big vector of different characters and then parses it up into 
passwords. The vector can be duplicated over and over by making the seed the master password.

  master.password=29852758
  passwords(master.password,sites=c("Work","Bank","Investment","gmail.com","yahoo.com","hotmail.com","visa","spotify"),changes=c(1,1,1,1,1,1,2,1))
