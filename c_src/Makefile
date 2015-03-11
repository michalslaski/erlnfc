readNFC : readNFC.o nfc.o wiringPiSPI.o wiringPi.o piHiPri.o piThread.o
	@gcc readNFC.o nfc.o wiringPiSPI.o wiringPi.o piHiPri.o piThread.o -o readNFC -lpthread -g
readNFC.o : readNFC.c nfc.h wiringPiSPI.h
	@gcc -c readNFC.c -g
nfc.o : nfc.c nfc.h wiringPiSPI.h
	@gcc -c nfc.c -g
wiringPiSPI.o : wiringPiSPI.c wiringPiSPI.h
	@gcc -c wiringPiSPI.c -g
wiringPi.o : wiringPi.c wiringPi.h
	@gcc -c wiringPi.c -g
piHiPri.o : piHiPri.c
	@gcc -c piHiPri.c -g
piThread.o : piThread.c
	@gcc -c piThread.c -g
clean :
	@rm *.o readNFC
libNFC.so : 
	@cc -shared -fpic -o libNFC.so nfc.c wiringPiSPI.c wiringPi.c piHiPri.c piThread.c -lpthread
install :
	@sudo mv libNFC.so /usr/lib
