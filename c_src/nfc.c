#include "nfc.h"

//#define PN532DEBUG

byte pn532ack[] = {
  0x00, 0x00, 0xFF, 0x00, 0xFF, 0x00};
byte pn532response_firmwarevers[] = {
  0x00, 0xFF, 0x06, 0xFA, 0xD5, 0x03};

#define PN532_PACK_BUFF_SIZE 64
#define SPISPEED 1000000

byte pn532_packetbuffer[PN532_PACK_BUFF_SIZE];

/*Construct PN532 object and construct PN532's own SPI interface object */
int i,j;
unsigned char a=0xaa;
void begin()
{
	j = wiringPiSetup();
	wiringPiSPISetup(channel,SPISPEED);

	pinMode(_cs,OUTPUT);

	digitalWrite(_cs,HIGH);
	digitalWrite(rst,HIGH);
	digitalWrite(_cs, LOW);

	delay(1000);
//	for(;;){write(0x01);}
	pn532_packetbuffer[0] = PN532_FIRMWAREVERSION;
//	for(;;){
	sendCommandCheckAck(pn532_packetbuffer, 1, 1000);
//	}
/*	if(i == 0)
	{
		printf("the ACK return is false\n");
	}
	else
	{
		printf("the ACK return is ture\n");
	}
*/
}


uint32_t getFirmwareVersion(void)
{
	uint32_t response;

	pn532_packetbuffer[0] = PN532_FIRMWAREVERSION;

	if (!sendCommandCheckAck(pn532_packetbuffer, 1, 1000))
	return 0;

	//Read data packet
	read(pn532_packetbuffer, 12);
	//Check some basic stuff
	if (0 != strncmp((char *)pn532_packetbuffer, (char *)pn532response_firmwarevers, 6)) 
	{
		return 0;
	}

	response = pn532_packetbuffer[6];
	response <<= 8;
	response |= pn532_packetbuffer[7];
	response <<= 8;
	response |= pn532_packetbuffer[8];
	response <<= 8;
	response |= pn532_packetbuffer[9];

	return response;
}

/**********************************************************************/
/*Function: Send command to PN532 with SPI and check the ACK.          */
/*Parameter:-uint8_t* cmd,The pointer that saves the command code to be sent;*/
/*          -uint8_t cmd_len,The number of bytes of the command;        */
/*	    -uint16_t timeout,default timeout is one second            */
/*Return:   boolean,ture = send command successfully	               */
boolean sendCommandCheckAck(uint8_t* cmd, uint8_t cmd_len, uint16_t timeout) 
{
	uint16_t timer = 0;

	// write the command
	writeCommand(cmd, cmd_len);
//	printf("command have been send\n");
	// Wait for chip to say it's ready!
	while (readSpiStatus() != PN532_SPI_READY)
	{
		if (timeout != 0)
		{
			timer+=10;
			if (timer > timeout)
			{
//			printf("timeout\n");
			return false;
			}
		}
		delay(10);
	}
//	printf("read spi finsh\n");
	// read acknowledgement
	if (!checkSpiAck())
	{
//		printf("spi no answer\n");
		return false;
	}

	timer = 0;
//	printf("check spi finsh\n");
	// Wait for chip to say its ready!
	while (readSpiStatus() != PN532_SPI_READY)
	{
		if (timeout != 0)
		{
			timer+=10;
			if (timer > timeout)
//			printf("read spi timeout\n");
			return false;
		}
		delay(10);
	}
//	printf("the spi return ture\n");
	return true; // ack'd command
}

boolean SAMConfig(void) 
{
	pn532_packetbuffer[0] = PN532_SAMCONFIGURATION;
	pn532_packetbuffer[1] = 0x01; // normal mode;
	pn532_packetbuffer[2] = 0x14; // timeout 50ms * 20 = 1 second
	pn532_packetbuffer[3] = 0x01; // use IRQ pin!

	if (! sendCommandCheckAck(pn532_packetbuffer, 4,1000))
	return false;

	// read data packet
	read(pn532_packetbuffer, 8);

	return  (pn532_packetbuffer[5] == 0x15);
}

/**********************************************************************/
/*Function: Configure the NFC shield as initiator in the peer to peer */
/*	      commnunication and only the initiator set the baud rate.*/
/*Parameter:-uint8_t baudrate,Any number from 0-2. 0 for 106kbps or 
 	    1 for 201kbps or 2 for 424kbps but 106kps is not supported yet;*/
/*Return: boolean,ture = the shield finds the target and is configured */
/*	as initiator successfully.                      	       */
uint32_t configurePeerAsInitiator(uint8_t baudrate) 
{
	uint8_t i;

	pn532_packetbuffer[0] = PN532_INJUMPFORDEP;
	pn532_packetbuffer[1] = 0x01; 		//Active Mode
	pn532_packetbuffer[2] = baudrate;	// Use 1 or 2. //0 i.e 106kps is not supported yet
	pn532_packetbuffer[3] = 0x01; 		//Indicates Optional Payload is present

	//Polling request payload
	pn532_packetbuffer[4] = 0x00;
	pn532_packetbuffer[5] = 0xFF;
	pn532_packetbuffer[6] = 0xFF;
	pn532_packetbuffer[7] = 0x00;
	pn532_packetbuffer[8] = 0x00;

	printf("begin to send command\n");

	if (!sendCommandCheckAck(pn532_packetbuffer,9,1000))
	{
		printf("send baud no answer return flase\n");
		return false;
	}
	else
	{
		printf("send data finsh\n");
	}
	// read data packet
	read(pn532_packetbuffer, 19+6);

//#ifdef PN532DEBUG
	// check the response
	printf("PEER_INITIATOR:");

	for(i=0;i<19+6;i++)
	{
		printf("%d\n",(pn532_packetbuffer[i]));
	}
//#endif

	return (pn532_packetbuffer[7] == 0x00); //No error

}
/**********************************************************************/
/*Function: Transmit to the target and receive from the target.       */
/*Parameter:-char* dataOut,data buffer to send;                       */
/*	    -char* dataIn,data buffer to save the data receive.       */
/*Return:   boolean,ture = No error                                   */
boolean initiatorTxRx(char* dataOut,char* dataIn)
{
	uint8_t iter;
	uint8_t i;

	pn532_packetbuffer[0] = PN532_INDATAEXCHANGE;
	pn532_packetbuffer[1] = 0x01; //Target 01

	for(iter=(2+0);iter<(2+16);iter++)
	{
		pn532_packetbuffer[iter] = dataOut[iter-2]; //pack the data to send to target
	}

	if (! sendCommandCheckAck(pn532_packetbuffer, 18,1000))
	return false;

	// read data packet
	read(pn532_packetbuffer, 18+6);

	#ifdef PN532_P2P_DEBUG
	// check the response
	printf("INITIATOR receive:");
	for(i=0;i<18+6;i++)
	{
		printf("%d\n",(pn532_packetbuffer[i])); 
//		printf(" ");
	}
	printf("\n");
	#endif

	for(iter=8;iter<(8+16);iter++)
	{
		dataIn[iter-8] = pn532_packetbuffer[iter]; //data received from target
	}

	return (pn532_packetbuffer[7] == 0x00); //No error
}

uint32_t configurePeerAsTarget() 
{
	uint8_t i;

	byte pbuffer[38] = { 
	PN532_TGINITASTARGET, 
	0x00,
	0x08, 0x00, 		//SENS_RES
	0x12, 0x34, 0x56, 	//NFCID1
	0x40, 				//SEL_RES
	0x01, 0xFE, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 			// POL_RES
	0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 
	0xFF, 0xFF,
	0xAA, 0x99, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11, 
				//NFCID3t: Change this to desired value
	0x00, 0x00 	//Length of general and historical bytes
	};

	for(i=0;i<38;i++)
	{
		pn532_packetbuffer[i] = pbuffer[i];
	}
	if (! sendCommandCheckAck(pn532_packetbuffer, 38,1000))
	return false;

	// read data packet
	read(pn532_packetbuffer, 18+6);

#ifdef PN532DEBUG
	// check some basic stuff
	printf("PEER_TARGET");
	for(i=0;i<18+6;i++)
	{
		printf("%d\n",(pn532_packetbuffer[i])); 
		printf(" ");
	}
	printf("\n");
#endif

	return (pn532_packetbuffer[23] == 0x00); //No error as it received all response
}
/*Function: Recieve data first and then transmit data to the initiator*/
uint32_t targetTxRx(char* dataOut,char* dataIn)
{
	uint8_t i;
	uint8_t iter;

	/* Receiving from Initiator */
	pn532_packetbuffer[0] = PN532_TGGETDATA;
	if (! sendCommandCheckAck(pn532_packetbuffer, 1,1000))
	return false;

	// read data packet
	read(pn532_packetbuffer, 18+6);

#ifdef PN532_P2P_DEBUG
	printf("\n");
	// check the response
	printf("TARGET RX:");
	for(i=0;i<18+6;i++)
	{
		printf("%d\n",(pn532_packetbuffer[i])); 
		printf(" ");
	}
#endif

	for(iter=8;iter<(8+16);iter++)
	{
		dataIn[iter-8] = pn532_packetbuffer[iter]; //data received from initiator
	}

	/* Sending to Initiator */
	if(pn532_packetbuffer[7] == 0x00) //If no errors in receiving, send data.
	{
		pn532_packetbuffer[0] = PN532_TGSETDATA;
		for(iter=(1+0);iter<(1+16);iter++)
		{
			pn532_packetbuffer[iter] = dataOut[iter-1]; //pack the data to send to target
		}

		if (! sendCommandCheckAck(pn532_packetbuffer, 17,1000))
		return false;

		// read data packet
		read(pn532_packetbuffer, 2+6);

	#ifdef PN532_P2P_DEBUG

		// check the response
		printf("TARGET get response after transmiting: ");
		for(i=0;i<2+6;i++)
		{
			printf("%d\n",(pn532_packetbuffer[i])); 
//			printf(" ");
		}
		printf("\n");
	#endif

		return (pn532_packetbuffer[7] == 0x00); //No error
	}

}

uint32_t authenticateBlock(uint8_t cardnumber /*1 or 2*/,uint32_t cid /*Card NUID*/, uint8_t blockaddress /*0 to 63*/,uint8_t authtype/*Either KEY_A or KEY_B */, uint8_t * keys) 
{
	int iter;
	uint8_t i;

	pn532_packetbuffer[0] = PN532_INDATAEXCHANGE;
	pn532_packetbuffer[1] = cardnumber;  	// either card 1 or 2 (tested for card 1)

	if(authtype == KEY_A)
	{
		pn532_packetbuffer[2] = PN532_AUTH_WITH_KEYA;
	}
	else
	{
		pn532_packetbuffer[2] = PN532_AUTH_WITH_KEYB;
	}
	pn532_packetbuffer[3] = blockaddress; //This address can be 0-63 for MIFARE 1K card

	pn532_packetbuffer[4] = keys[0];
	pn532_packetbuffer[5] = keys[1];
	pn532_packetbuffer[6] = keys[2];
	pn532_packetbuffer[7] = keys[3];
	pn532_packetbuffer[8] = keys[4];
	pn532_packetbuffer[9] = keys[5];

	pn532_packetbuffer[10] = ((cid >> 24) & 0xFF);
	pn532_packetbuffer[11] = ((cid >> 16) & 0xFF);
	pn532_packetbuffer[12] = ((cid >> 8) & 0xFF);
	pn532_packetbuffer[13] = ((cid >> 0) & 0xFF);

	if (! sendCommandCheckAck(pn532_packetbuffer, 14,1000))
	return false;

	// read data packet
	read(pn532_packetbuffer, 2+6);

#ifdef PN532DEBUG
	for(iter=0;iter<14;iter++)
	{
		printf("%d\n",(pn532_packetbuffer[iter]));
//		printf(" ");
	}
	printf("\n");
	// check some basic stuff

	printf("AUTH");
	for(i=0;i<2+6;i++)
	{
		printf("%d\n",(pn532_packetbuffer[i]));
//		printf(" ");
	}
#endif

	if((pn532_packetbuffer[6] == 0x41) && (pn532_packetbuffer[7] == 0x00))
	{
		return true;
	}
	else
	{
		return false;
	}

}
/****************************************************************************/
/*Function: Read a block(16 bytes) from the tag and stores in the parameter.*/
/*Parameter:-uint8_t cardnumber,can be 1 or 2;                              */
/*          -blockaddress,range from 0 to 63;                               */
/*	    -uint8_t* block,will save 16bytes that read from tag.           */
/*Return:   boolean         					      	    */
boolean readMemoryBlock(uint8_t cardnumber,uint8_t blockaddress,uint8_t * block)
{
	uint8_t i;

	pn532_packetbuffer[0] = PN532_INDATAEXCHANGE;
	pn532_packetbuffer[1] = cardnumber;  // either card 1 or 2 (tested for card 1)
	pn532_packetbuffer[2] = PN532_MIFARE_READ;
	pn532_packetbuffer[3] = blockaddress; //This address can be 0-63 for MIFARE 1K card

	if (! sendCommandCheckAck(pn532_packetbuffer, 4,1000))
	return false;

	// read data packet
	read(pn532_packetbuffer, 18+6);
	// check some basic stuff
#ifdef PN532DEBUG
	printf("READ");
#endif
	for(i=8;i<18+6;i++)
	{
		block[i-8] = pn532_packetbuffer[i];
#ifdef PN532DEBUG
		printf("%d\n",(pn532_packetbuffer[i])); 
//		printf(" ");
#endif
	}
	printf("\n");
	if((pn532_packetbuffer[6] == 0x41) && (pn532_packetbuffer[7] == 0x00))
	{
		return true; 		//read successful
	}
	else
	{
		return false;
	}

}

/****************************************************************************/
/*Function: Write a block(16 bytes) to the tag.                             */
/*Parameter:-uint8_t cardnumber,can be 1 or 2;                              */
/*          -blockaddress,range from 0 to 63;                               */
/*	    -uint8_t* block,saves 16bytes that will write to the tag.       */
/*Return:  boolean							    */
/*Note:Donot write to Sector Trailer Block unless you know what you are doing.*/
boolean writeMemoryBlock(uint8_t cardnumber,uint8_t blockaddress,uint8_t * block) 
{
	uint8_t bytes;
	uint8_t i;

	pn532_packetbuffer[0] = PN532_INDATAEXCHANGE;
	pn532_packetbuffer[1] = cardnumber;  		// either card 1 or 2 (tested for card 1)
	pn532_packetbuffer[2] = PN532_MIFARE_WRITE;
	pn532_packetbuffer[3] = blockaddress;

	for(bytes = 0; bytes < 16; bytes++)
	{
		pn532_packetbuffer[4+bytes] = block[bytes];
	}

	if (! sendCommandCheckAck(pn532_packetbuffer, 20,1000))
	return false;
	// read data packet
	read(pn532_packetbuffer, 2+6);

#ifdef PN532DEBUG
	// check some basic stuff
	printf("WRITE");

	for(i=0;i<2+6;i++)
	{
		printf("%d\n",(pn532_packetbuffer[i])); 

	}
	printf("\n");
#endif

	if((pn532_packetbuffer[6] == 0x41) && (pn532_packetbuffer[7] == 0x00))
	{
		return true; 									//write successful
	}
	else
	{
		return false;
	}
}

uint32_t readPassiveTargetID(uint8_t cardbaudrate) 
{
	uint32_t cid;
	uint16_t sens_res;
	uint8_t i;

	pn532_packetbuffer[0] = PN532_INLISTPASSIVETARGET;
	pn532_packetbuffer[1] = 1;  	// max 1 cards at once (we can set this to 2 later)
	pn532_packetbuffer[2] = cardbaudrate;

	if (! sendCommandCheckAck(pn532_packetbuffer, 3,1000))
	return 0x0;  					// no cards read

	// read data packet
	read(pn532_packetbuffer, 20);
	// check some basic stuff

	if (pn532_packetbuffer[7] != 1)
	return 0;

	sens_res = pn532_packetbuffer[9];
	sens_res <<= 8;
	sens_res |= pn532_packetbuffer[10];
	cid = 0;

	for (i=0; i< pn532_packetbuffer[12]; i++)
	{
		cid <<= 8;
		cid |= pn532_packetbuffer[13+i];
	}

	return cid;
}

/**********************************************************************/
/*Function: Read n bytes data and it stores in the parameter .        	*/
/*Parameter:-uint8_t* buff,saves the data read from PN532;            	*/
/*	    -uint8_t n,tells it wll read n bytes.                     		*/
/*Return:  void                                                       	*/
void read(uint8_t* buff, uint8_t n) 
{
	uint8_t i;

	digitalWrite(_cs, LOW);
	delay(2);
	write(PN532_SPI_DATAREAD);

#ifdef PN532DEBUG
	printf("Reading:\n");
#endif

	for (i=0; i < n; i ++) 
	{
		delay(1);
		buff[i] = readF();
#ifdef PN532DEBUG
		printf("debug readf is %d\n",buff[i]);
#endif
	}
	digitalWrite(_cs, HIGH);
}

void writeCommand(uint8_t* cmd, uint8_t cmd_len)
{
	uint8_t checksum;
	uint8_t cmdlen_1;
	uint8_t i;
	uint8_t checksum_1;

	cmd_len++;

#ifdef PN532DEBUG
	printf("Sending: \n");
#endif

	digitalWrite(_cs, LOW);
	delay(2);     				// or whatever the delay is for waking up the board

	write(PN532_SPI_DATAWRITE); 	//0x01
//		printf("%d\n",PN532_SPI_DATAWRITE);

	checksum = PN532_PREAMBLE + PN532_PREAMBLE + PN532_STARTCODE2;
	write(PN532_PREAMBLE);		//0x00
	write(PN532_PREAMBLE);		//0x00
	write(PN532_STARTCODE2);	//0xff

	write(cmd_len);			//0x02
	cmdlen_1=~cmd_len + 1;
	write(cmdlen_1);		//0x01

	write(PN532_HOSTTOPN532);	//0xd4
	checksum += PN532_HOSTTOPN532;

#ifdef PN532DEBUG
//	printf(" 0x");
	printf("preamble is %d\n",(PN532_PREAMBLE));
//	printf(" 0x");
	printf("preambls is %d\n",(PN532_PREAMBLE));
//	printf(" 0x");
	printf("startcode2 is %d\n",(PN532_STARTCODE2));
//	printf(" 0x");
	printf("cmd_len is %d\n",(cmd_len));
//	printf(" 0x");
	printf("cmdlen_1 is %d\n",(cmdlen_1));
//	printf(" 0x");
	printf("hosttopn532 is %d\n",(PN532_HOSTTOPN532));
//	printf("\n");
#endif

	for (i=0; i<cmd_len-1; i++) 
	{
		write(cmd[i]);
		checksum += cmd[i];
#ifdef PN532DEBUG
		printf("cmd[i] is %d\n",(cmd[i]));
#endif
	}

	checksum_1=~checksum;
	write(checksum_1);
	write(PN532_POSTAMBLE);
	digitalWrite(_cs, HIGH);

#ifdef PN532DEBUG
	printf("checksum is %d\n",(checksum_1));
	printf("postamble is %d\n",(PN532_POSTAMBLE));
#endif
}
/************** high level SPI */
boolean checkSpiAck()
{
	uint8_t ackbuff[6];
	read(ackbuff, 6);
	return (0 == strncmp((char *)ackbuff, (char *)pn532ack, 6));
}

/************** mid level SPI */
uint8_t readSpiStatus(void) 
{
	uint8_t status;

	digitalWrite(_cs, LOW);
	delay(2);
	write(PN532_SPI_STATREAD);
	status = readF();
	digitalWrite(_cs, HIGH);
	return status;
}

/************** low level SPI ********/
/*Function:Transmit a byte to PN532 through the SPI interface. */
void write(uint8_t _data)
{
	unsigned char wdata,rdata,p,pp,sdata;
	unsigned long long m,n;
	unsigned long long *i,*j;
	sdata = 0x00;

	for(p=0;p<8;p++)
	{

		if(_data & 0x01)
		{
			sdata |= 1<<(7-p);
		}
		else
		{

		}
		_data = _data>>1;
	}

	wdata = sdata;
	rdata = 0x00;
	m = (unsigned long long)wdata;
	n = (unsigned long long)rdata;
	i = &m;
	j = &n;

	wiringPiSPIDataRW(1,i,j,sizeof(wdata));

}

/*Function:Receive a byte from PN532 through the SPI interface */
uint8_t readF(void)
{
	uint8_t data_,redata,p,pp;
	unsigned char wdata,rdata;
	unsigned long long m,n;
	unsigned long long *i,*j;

	m = (unsigned long long)wdata;
	n = (unsigned long long)rdata;
	i = &m;
	j = &n;
	redata = 0;
	wiringPiSPIDataRW(1,i,j,1);
	data_= *j ;

	for(p=0;p<8;p++)
	{
		if(data_ & 0x01)
		{
			redata |= 1<<(7-p);
			pp=1;
		}
		else
		{
			pp=0;
		}
		data_ = data_>>1;
	}
	return redata;
}

