/* 
 * Copyright (C) 2012-2014 Chris McClelland
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <makestuff.h>
#include <libfpgalink.h>
#include <libbuffer.h>
#include <liberror.h>
#include <libdump.h>
#include <argtable2.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <fcntl.h> 
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <time.h>

#ifdef WIN32
#include <Windows.h>
#else
#include <sys/time.h>
#endif
#define CR     "\x1b[31m"
#define CG  "\x1b[32m"
#define CY  "\x1b[33m"
#define CB  "\x1b[34m"
#define CM "\x1b[35m"
#define CC "\x1b[36m"
#define RST "\x1b[0m"

////////////////////////////////////////////////////////////////////////////////////////////////////
// global declarations 
char* key="11001100110011001100110011000001"; // initialise it 
char line1[100];
char* ack1="11110000111100001111000011110000"; 
char* ack2="00001111000011110000111100001111"; 
char *portname = "/dev/ttyXRUSB0" ;
int fd; // File descriptor corresponding to the USB Port 
////////////////////////////////////////////////////////////////////////////////////////////////////


bool sigIsRaised(void);
void sigRegisterHandler(void);

static const char *ptr;
static bool enableBenchmarking = false;


long timediff(clock_t t1, clock_t t2) {
    long elapsed;
    elapsed = ((double)t2 - t1) / CLOCKS_PER_SEC * 1000;
    return elapsed;
}


void save_to_file(int table[][5], int m){
	
	FILE *fp;
	 
	int i,j;
	 
	char* filename=strcat("track_data",".csv");
	 
	fp=fopen(filename,"w");
	 	 
	for(i=0;i<m;i++){
	 
	    fprintf(fp,"\n%d",i+1);
	 
	    for(j=0;j<5;j++)
	 
	        fprintf(fp,",%d ",table[i][j]);
	 
	    }
	 
	fclose(fp);
 
 	return ;
}


int set_interface_attribs (int fd, int speed, int parity)
{
        struct termios tty;
        memset (&tty, 0, sizeof tty);
        if (tcgetattr (fd, &tty) != 0)
        {
                printf("error %d from tcgetattr", errno);
                return -1;
        }

        cfsetospeed (&tty, speed);
        cfsetispeed (&tty, speed);

        tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS8;     // 8-bit chars
        // disable IGNBRK for mismatched speed tests; otherwise receive break
        // as \000 chars
        tty.c_iflag &= ~IGNBRK;         // disable break processing
        tty.c_lflag = 0;                // no signaling chars, no echo,
                                        // no canonical processing
        tty.c_oflag = 0;                // no remapping, no delays
        tty.c_cc[VMIN]  = 0;            // read doesn't block
        tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

        tty.c_iflag &= ~(IXON | IXOFF | IXANY); // shut off xon/xoff ctrl

        tty.c_cflag |= (CLOCAL | CREAD);// ignore modem controls,
                                        // enable reading
        tty.c_cflag &= ~(PARENB | PARODD);      // shut off parity
        tty.c_cflag |= parity;
        tty.c_cflag &= ~CSTOPB;
       // tty.c_cflag &= ~CRTSCTS;
    

        if (tcsetattr (fd, TCSANOW, &tty) != 0)
        {
                printf ("error %d from tcsetattr", errno);
                return -1;
        }
        return 0;
}

void set_blocking (int fd, int should_block)
{
        struct termios tty;
        memset (&tty, 0, sizeof tty);
        if (tcgetattr (fd, &tty) != 0)
        {
                printf ("error %d from tggetattr", errno);
                return;
        }

        tty.c_cc[VMIN]  = should_block ? 1 : 0;
        tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

        if (tcsetattr (fd, TCSANOW, &tty) != 0)
               printf ("error %d setting term attributes", errno);
} 


char* read_one_byte_from_uart(long long timeout){
        

   // clock_t start;	

    if (fd < 0)
    {
        printf ("error %d opening %s: %s", errno, portname, strerror (errno));
        return 0;
    }
    
    set_interface_attribs (fd, B2400, 0);  // set speed to 115,200 bps, 8n1 (no parity)
    set_blocking (fd, 0);                // set no blocking
    char buf[1];
    char *ans=malloc(9); 

	//start = clock();    

   // while (timediff(clock(), start) < timeout*1000) {
    	int n = read (fd, buf, sizeof(buf)); 
    	if (n == 1) {
	   // 	printf("N is =%d", n);
	    //	printf("The read string is %hhx\n",buf[0]);
	    	uint8 temp=buf[0]; 
            ans[0]='\0'; 
       //     printf("data read from uart in uint8 %d \n", temp); 
            while(temp>0){
                if(temp%2==1) { strcat(ans,"1"); }
                else strcat(ans,"0"); 
                temp>>=1; 
            }
	    	return ans;
	    }
	//}
	ans[0]='f'; return ans; 
}

int write_one_byte_to_uart(){
	    
	    unsigned char data[1];
	    printf(">> Enter data to Write via Uart:\n>>>> ");
    	scanf("%hhx", &data);

    	// error checking 
    	int n = write (fd, data, 1); 
 		if (n!=1)
    	{
        	//printf ("error in writing to the board through UART port");
        	return 0;
    	}
        //printf("Write Success\n");
    	return 1;

}

static bool isHexDigit(char ch) {
	return
		(ch >= '0' && ch <= '9') ||
		(ch >= 'a' && ch <= 'f') ||
		(ch >= 'A' && ch <= 'F');
}

static uint16 calcChecksum(const uint8 *data, size_t length) {
	uint16 cksum = 0x0000;
	while ( length-- ) {
		cksum = (uint16)(cksum + *data++);
	}
	return cksum;
}

static bool getHexNibble(char hexDigit, uint8 *nibble) {
	if ( hexDigit >= '0' && hexDigit <= '9' ) {
		*nibble = (uint8)(hexDigit - '0');
		return false;
	} else if ( hexDigit >= 'a' && hexDigit <= 'f' ) {
		*nibble = (uint8)(hexDigit - 'a' + 10);
		return false;
	} else if ( hexDigit >= 'A' && hexDigit <= 'F' ) {
		*nibble = (uint8)(hexDigit - 'A' + 10);
		return false;
	} else {
		return true;
	}
}

static int getHexByte(uint8 *byte) {
	uint8 upperNibble;
	uint8 lowerNibble;
	if ( !getHexNibble(ptr[0], &upperNibble) && !getHexNibble(ptr[1], &lowerNibble) ) {
		*byte = (uint8)((upperNibble << 4) | lowerNibble);
		byte += 2;
		return 0;
	} else {
    printf(" after dump \n "); 
		return 1;
	}
}

static const char *const errMessages[] = {
	NULL,
	NULL,
	"Unparseable hex number",
	"Channel out of range",
	"Conduit out of range",
	"Illegal character",
	"Unterminated string",
	"No memory",
	"Empty string",
	"Odd number of digits",
	"Cannot load file",
	"Cannot save file",
	"Bad arguments"
};

typedef enum {
	FLP_SUCCESS,
	FLP_LIBERR,
	FLP_BAD_HEX,
	FLP_CHAN_RANGE,
	FLP_CONDUIT_RANGE,
	FLP_ILL_CHAR,
	FLP_UNTERM_STRING,
	FLP_NO_MEMORY,
	FLP_EMPTY_STRING,
	FLP_ODD_DIGITS,
	FLP_CANNOT_LOAD,
	FLP_CANNOT_SAVE,
	FLP_ARGS
} ReturnCode;

static ReturnCode doRead(
	struct FLContext *handle, uint8 chan, uint32 length, FILE *destFile, uint16 *checksum,
	const char **error)
{
	ReturnCode retVal = FLP_SUCCESS;
	uint32 bytesWritten;
	FLStatus fStatus;
	uint32 chunkSize;
	const uint8 *recvData;
	uint32 actualLength;
	const uint8 *ptr;
	uint16 csVal = 0x0000;
	#define READ_MAX 65536

	// Read first chunk
	chunkSize = length >= READ_MAX ? READ_MAX : length;
	fStatus = flReadChannelAsyncSubmit(handle, chan, chunkSize, NULL, error);
	CHECK_STATUS(fStatus, FLP_LIBERR, cleanup, "doRead()");
	length = length - chunkSize;

	while ( length ) {
		// Read chunk N
		chunkSize = length >= READ_MAX ? READ_MAX : length;
		fStatus = flReadChannelAsyncSubmit(handle, chan, chunkSize, NULL, error);
		CHECK_STATUS(fStatus, FLP_LIBERR, cleanup, "doRead()");
		length = length - chunkSize;
		
		// Await chunk N-1
		fStatus = flReadChannelAsyncAwait(handle, &recvData, &actualLength, &actualLength, error);
		CHECK_STATUS(fStatus, FLP_LIBERR, cleanup, "doRead()");

		// Write chunk N-1 to file
		bytesWritten = (uint32)fwrite(recvData, 1, actualLength, destFile);
		CHECK_STATUS(bytesWritten != actualLength, FLP_CANNOT_SAVE, cleanup, "doRead()");

		// Checksum chunk N-1
		chunkSize = actualLength;
		ptr = recvData;
		while ( chunkSize-- ) {
			csVal = (uint16)(csVal + *ptr++);
		}
	}

	// Await last chunk
	fStatus = flReadChannelAsyncAwait(handle, &recvData, &actualLength, &actualLength, error);
	CHECK_STATUS(fStatus, FLP_LIBERR, cleanup, "doRead()");
	
	// Write last chunk to file
	bytesWritten = (uint32)fwrite(recvData, 1, actualLength, destFile);
	CHECK_STATUS(bytesWritten != actualLength, FLP_CANNOT_SAVE, cleanup, "doRead()");

	// Checksum last chunk
	chunkSize = actualLength;
	ptr = recvData;
	while ( chunkSize-- ) {
		csVal = (uint16)(csVal + *ptr++);
	}
	
	// Return checksum to caller
	*checksum = csVal;
cleanup:
	return retVal;
}

static ReturnCode doWrite(
	struct FLContext *handle, uint8 chan, FILE *srcFile, size_t *length, uint16 *checksum,
	const char **error)
{
	ReturnCode retVal = FLP_SUCCESS;
	size_t bytesRead, i;
	FLStatus fStatus;
	const uint8 *ptr;
	uint16 csVal = 0x0000;
	size_t lenVal = 0;
	#define WRITE_MAX (65536 - 5)
	uint8 buffer[WRITE_MAX];

	do {
		// Read Nth chunk
		bytesRead = fread(buffer, 1, WRITE_MAX, srcFile);
		if ( bytesRead ) {
			// Update running total
			lenVal = lenVal + bytesRead;

			// Submit Nth chunk
			fStatus = flWriteChannelAsync(handle, chan, bytesRead, buffer, error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup, "doWrite()");

			// Checksum Nth chunk
			i = bytesRead;
			ptr = buffer;
			while ( i-- ) {
				csVal = (uint16)(csVal + *ptr++);
			}
		}
	} while ( bytesRead == WRITE_MAX );

	// Wait for writes to be eeceived. This is optional, but it's only fair if we're benchmarking to
	// actually wait for the work to be completed.
	fStatus = flAwaitAsyncWrites(handle, error);
	CHECK_STATUS(fStatus, FLP_LIBERR, cleanup, "doWrite()");

	// Return checksum & length to caller
	*checksum = csVal;
	*length = lenVal;
cleanup:
	return retVal;
}

uint8 out;

static int parseLine(struct FLContext *handle, const char *line, const char **error) {
	ReturnCode retVal = FLP_SUCCESS, status;
	FLStatus fStatus;
	struct Buffer dataFromFPGA = {0,};
	BufferStatus bStatus;
	uint8 *data = NULL;
	char *fileName = NULL;
	FILE *file = NULL;
	double totalTime, speed;
	#ifdef WIN32
		LARGE_INTEGER tvStart, tvEnd, freq;
		DWORD_PTR mask = 1;
		SetThreadAffinityMask(GetCurrentThread(), mask);
		QueryPerformanceFrequency(&freq);
	#else
		struct timeval tvStart, tvEnd;
		long long startTime, endTime;
	#endif
	bStatus = bufInitialise(&dataFromFPGA, 1024, 0x00, error);
	CHECK_STATUS(bStatus, FLP_LIBERR, cleanup);
	ptr = line;
	do {
		while ( *ptr == ';' ) {
			ptr++;
		}
		switch ( *ptr ) {
		case 'r':{
			uint32 chan;
			uint32 length = 1;
			char *end;
			ptr++;
			
			// Get the channel to be read:
			errno = 0;
			chan = (uint32)strtoul(ptr, &end, 16);
			CHECK_STATUS(errno, FLP_BAD_HEX, cleanup);

			// Ensure that it's 0-127
			CHECK_STATUS(chan > 127, FLP_CHAN_RANGE, cleanup);
			ptr = end;

			// Only three valid chars at this point:
			CHECK_STATUS(*ptr != '\0' && *ptr != ';' && *ptr != ' ', FLP_ILL_CHAR, cleanup);

			if ( *ptr == ' ' ) {
				ptr++;

				// Get the read count:
				errno = 0;
				length = (uint32)strtoul(ptr, &end, 16);
				CHECK_STATUS(errno, FLP_BAD_HEX, cleanup);
				ptr = end;
				
				// Only three valid chars at this point:
				CHECK_STATUS(*ptr != '\0' && *ptr != ';' && *ptr != ' ', FLP_ILL_CHAR, cleanup);
				if ( *ptr == ' ' ) {
					const char *p;
					const char quoteChar = *++ptr;
					CHECK_STATUS(
						(quoteChar != '"' && quoteChar != '\''),
						FLP_ILL_CHAR, cleanup);
					
					// Get the file to write bytes to:
					ptr++;
					p = ptr;
					while ( *p != quoteChar && *p != '\0' ) {
						p++;
					}
					CHECK_STATUS(*p == '\0', FLP_UNTERM_STRING, cleanup);
					fileName = malloc((size_t)(p - ptr + 1));
					CHECK_STATUS(!fileName, FLP_NO_MEMORY, cleanup);
					CHECK_STATUS(p - ptr == 0, FLP_EMPTY_STRING, cleanup);
					strncpy(fileName, ptr, (size_t)(p - ptr));
					fileName[p - ptr] = '\0';
					ptr = p + 1;
				}
			}
			if ( fileName ) {
				uint16 checksum = 0x0000;

				// Open file for writing
				file = fopen(fileName, "wb");
				CHECK_STATUS(!file, FLP_CANNOT_SAVE, cleanup);
				free(fileName);
				fileName = NULL;

				#ifdef WIN32
					QueryPerformanceCounter(&tvStart);
					status = doRead(handle, (uint8)chan, length, file, &checksum, error);
					QueryPerformanceCounter(&tvEnd);
					totalTime = (double)(tvEnd.QuadPart - tvStart.QuadPart);
					totalTime /= freq.QuadPart;
					speed = (double)length / (1024*1024*totalTime);
				#else
					gettimeofday(&tvStart, NULL);
					status = doRead(handle, (uint8)chan, length, file, &checksum, error);
					gettimeofday(&tvEnd, NULL);
					startTime = tvStart.tv_sec;
					startTime *= 1000000;
					startTime += tvStart.tv_usec;
					endTime = tvEnd.tv_sec;
					endTime *= 1000000;
					endTime += tvEnd.tv_usec;
					totalTime = (double)(endTime - startTime);
					totalTime /= 1000000;  // convert from uS to S.
					speed = (double)length / (1024*1024*totalTime);
				#endif
				if ( enableBenchmarking ) {
					printf(
						"Read %d bytes (checksum 0x%04X) from channel %d at %f MiB/s\n",
						length, checksum, chan, speed);
				}
				CHECK_STATUS(status, status, cleanup);

				// Close the file
				fclose(file);
				file = NULL;
			} else {
				size_t oldLength = dataFromFPGA.length;
				bStatus = bufAppendConst(&dataFromFPGA, 0x00, length, error);
				CHECK_STATUS(bStatus, FLP_LIBERR, cleanup);
				#ifdef WIN32
					QueryPerformanceCounter(&tvStart);
					fStatus = flReadChannel(handle, (uint8)chan, length, dataFromFPGA.data + oldLength, error);
					QueryPerformanceCounter(&tvEnd);
					totalTime = (double)(tvEnd.QuadPart - tvStart.QuadPart);
					totalTime /= freq.QuadPart;
					speed = (double)length / (1024*1024*totalTime);
				#else
					gettimeofday(&tvStart, NULL);
					fStatus = flReadChannel(handle, (uint8)chan, length, dataFromFPGA.data + oldLength, error);
                    out = dataFromFPGA.data[0];
					gettimeofday(&tvEnd, NULL);
					startTime = tvStart.tv_sec;
					startTime *= 1000000;
					startTime += tvStart.tv_usec;
					endTime = tvEnd.tv_sec;
					endTime *= 1000000;
					endTime += tvEnd.tv_usec;
					totalTime = (double)(endTime - startTime);
					totalTime /= 1000000;  // convert from uS to S.
					speed = (double)length / (1024*1024*totalTime);
				#endif
				if ( enableBenchmarking ) {
					printf(
						"Read %d bytes (checksum 0x%04X) from channel %d at %f MiB/s\n",
						length, calcChecksum(dataFromFPGA.data + oldLength, length), chan, speed);
				}
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			}
			break;
		}
		case 'w':{
			unsigned long int chan;
			size_t length = 1, i;
			char *end, ch;
			const char *p;
			ptr++;
			
			// Get the channel to be written:
			errno = 0;
			chan = strtoul(ptr, &end, 16);
			CHECK_STATUS(errno, FLP_BAD_HEX, cleanup);

			// Ensure that it's 0-127
			CHECK_STATUS(chan > 127, FLP_CHAN_RANGE, cleanup);
			ptr = end;

			// There must be a space now:
			CHECK_STATUS(*ptr != ' ', FLP_ILL_CHAR, cleanup);

			// Now either a quote or a hex digit
		   ch = *++ptr;
			if ( ch == '"' || ch == '\'' ) {
				uint16 checksum = 0x0000;

				// Get the file to read bytes from:
				ptr++;
				p = ptr;
				while ( *p != ch && *p != '\0' ) {
					p++;
				}
				CHECK_STATUS(*p == '\0', FLP_UNTERM_STRING, cleanup);
				fileName = malloc((size_t)(p - ptr + 1));
				CHECK_STATUS(!fileName, FLP_NO_MEMORY, cleanup);
				CHECK_STATUS(p - ptr == 0, FLP_EMPTY_STRING, cleanup);
				strncpy(fileName, ptr, (size_t)(p - ptr));
				fileName[p - ptr] = '\0';
				ptr = p + 1;  // skip over closing quote

				// Open file for reading
				file = fopen(fileName, "rb");
				CHECK_STATUS(!file, FLP_CANNOT_LOAD, cleanup);
				free(fileName);
				fileName = NULL;
				
				#ifdef WIN32
					QueryPerformanceCounter(&tvStart);
					status = doWrite(handle, (uint8)chan, file, &length, &checksum, error);
					QueryPerformanceCounter(&tvEnd);
					totalTime = (double)(tvEnd.QuadPart - tvStart.QuadPart);
					totalTime /= freq.QuadPart;
					speed = (double)length / (1024*1024*totalTime);
				#else
					gettimeofday(&tvStart, NULL);
					status = doWrite(handle, (uint8)chan, file, &length, &checksum, error);
					gettimeofday(&tvEnd, NULL);
					startTime = tvStart.tv_sec;
					startTime *= 1000000;
					startTime += tvStart.tv_usec;
					endTime = tvEnd.tv_sec;
					endTime *= 1000000;
					endTime += tvEnd.tv_usec;
					totalTime = (double)(endTime - startTime);
					totalTime /= 1000000;  // convert from uS to S.
					speed = (double)length / (1024*1024*totalTime);
				#endif
				if ( enableBenchmarking ) {
					printf(
						"Wrote "PFSZD" bytes (checksum 0x%04X) to channel %lu at %f MiB/s\n",
						length, checksum, chan, speed);
				}
				CHECK_STATUS(status, status, cleanup);

				// Close the file
				fclose(file);
				file = NULL;
			} else if ( isHexDigit(ch) ) {
				// Read a sequence of hex bytes to write
				uint8 *dataPtr;
				p = ptr + 1;
				while ( isHexDigit(*p) ) {
					p++;
				}
				CHECK_STATUS((p - ptr) & 1, FLP_ODD_DIGITS, cleanup);
				length = (size_t)(p - ptr) / 2;
				data = malloc(length);
				dataPtr = data;
				for ( i = 0; i < length; i++ ) {
					getHexByte(dataPtr++);
					ptr += 2;
				}
				#ifdef WIN32
					QueryPerformanceCounter(&tvStart);
					fStatus = flWriteChannel(handle, (uint8)chan, length, data, error);
					QueryPerformanceCounter(&tvEnd);
					totalTime = (double)(tvEnd.QuadPart - tvStart.QuadPart);
					totalTime /= freq.QuadPart;
					speed = (double)length / (1024*1024*totalTime);
				#else
					gettimeofday(&tvStart, NULL);
					fStatus = flWriteChannel(handle, (uint8)chan, length, data, error);
					gettimeofday(&tvEnd, NULL);
					startTime = tvStart.tv_sec;
					startTime *= 1000000;
					startTime += tvStart.tv_usec;
					endTime = tvEnd.tv_sec;
					endTime *= 1000000;
					endTime += tvEnd.tv_usec;
					totalTime = (double)(endTime - startTime);
					totalTime /= 1000000;  // convert from uS to S.
					speed = (double)length / (1024*1024*totalTime);
				#endif
				if ( enableBenchmarking ) {
					printf(
						"Wrote "PFSZD" bytes (checksum 0x%04X) to channel %lu at %f MiB/s\n",
						length, calcChecksum(data, length), chan, speed);
				}
				CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
				free(data);
				data = NULL;
			} else {
				FAIL(FLP_ILL_CHAR, cleanup);
			}
			break;
		}
		case '+':{
			uint32 conduit;
			char *end;
			ptr++;

			// Get the conduit
			errno = 0;
			conduit = (uint32)strtoul(ptr, &end, 16);
			CHECK_STATUS(errno, FLP_BAD_HEX, cleanup);

			// Ensure that it's 0-127
			CHECK_STATUS(conduit > 255, FLP_CONDUIT_RANGE, cleanup);
			ptr = end;

			// Only two valid chars at this point:
			CHECK_STATUS(*ptr != '\0' && *ptr != ';', FLP_ILL_CHAR, cleanup);

			fStatus = flSelectConduit(handle, (uint8)conduit, error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			break;
		}
		default:
			FAIL(FLP_ILL_CHAR, cleanup);
		}
	} while ( *ptr == ';' );
	CHECK_STATUS(*ptr != '\0', FLP_ILL_CHAR, cleanup);

	dump(0x00000000, dataFromFPGA.data, dataFromFPGA.length);

cleanup:
	bufDestroy(&dataFromFPGA);
	if ( file ) {
		fclose(file);
	}
	free(fileName);
	free(data);
	if ( retVal > FLP_LIBERR ) {
		const int column = (int)(ptr - line);
		int i;
		fprintf(stderr, "%s at column %d\n  %s\n  ", errMessages[retVal], column, line);
		for ( i = 0; i < column; i++ ) {
			fprintf(stderr, " ");
		}
		fprintf(stderr, "^\n");
	}
	return retVal;
}

static const char *nibbles[] = {
	"0000",  // '0'
	"0001",  // '1'
	"0010",  // '2'
	"0011",  // '3'
	"0100",  // '4'
	"0101",  // '5'
	"0110",  // '6'
	"0111",  // '7'
	"1000",  // '8'
	"1001",  // '9'

	"XXXX",  // ':'
	"XXXX",  // ';'
	"XXXX",  // '<'
	"XXXX",  // '='
	"XXXX",  // '>'
	"XXXX",  // '?'
	"XXXX",  // '@'

	"1010",  // 'A'
	"1011",  // 'B'
	"1100",  // 'C'
	"1101",  // 'D'
	"1110",  // 'E'
	"1111"   // 'F'
};

////////////////////////////////////////////////////////////////////////////////////////////////////
// Our own function declarations 



int read_table(const char * addr, int table[][5]) {
        printf("GOT INTO FUNNCTION%s", addr);
        FILE *file;
        file=fopen( addr , "r" );
        char c;
        int i=0,j=0;
        int nl;
        if(file){
                while( ( c=getc(file))!=EOF){
                        if(c == ' ' || c==',') continue;
                        else if( c >= '0' && c <= '9') { table[i][j]=c-48;  j++;  nl=i; }
                        else if ( c == '\n' ) { i++; j=0; }
                }
                fclose(file);
        }
        return nl+1;
}
char * mm= "0123456789abcdef";
char* boolToHex(char* input) // modify bool to hex , correct it 
{
    char * ans=malloc(9); 
    int var=(input[0]=='1'); 
    for(int i=1;i<32;i++){
        if(i%4==0){
            ans[i/4-1]=mm[var]; 
            var=(input[i]=='1'); 
        }
        else {
            if(input[i]=='1'){
                var+=(1<<(i%4)); 
            }
        }
    }
    ans[7]=mm[var]; 
	for(int i=0;i<4;i++){
			char temp=ans[2*i]; 
			ans[2*i] =ans[2*i+1]; 
			ans[2*i+1]=temp; 
	}
    ans[8]='\0'; 
    return ans; 
}

void get_coordinate(const char * xy, int *x , int *y){
        *x=0; *y=0; 
		for(int i=0;i<4;i++){
				if(xy[i]=='1') { *x+=(1<<i); }
		}
		for(int i=4;i<8;i++){
				if(xy[i]=='1') { *y+=(1<<(i-4)); }
		}
        printf("%d                       %d\n",*x,*y); 
}

char* func(int x, int y,  int table[][5], int rows){

	char *ret = malloc(65);

	bool *seen = malloc(8) ;

	for (int i=0;i<8;i++) seen[i] = false ;

  	for (int i=0; i<rows; i++){
   		if (table[i][0] == x && table[i][1] == y) {
   			int dir =  table[i][2] ;
   			int ns = table[i][4] ;
   			int j = 8*dir ;
   			ret [j+2] = ((dir/4)%2 == 1) ? '1' : '0'  ;
   			ret [j+3] = ((dir/2)%2 == 1) ? '1' : '0' ;
   			ret [j+4] = (dir%2 == 1) ? '1' : '0' ;
   			ret [j] = '1' ;
   			ret [j+1] = (table[i][3] == 1) ? '1' : '0' ;
   			ret [j+5] = ((ns/4)%2 == 1) ? '1' : '0' ;
   			ret [j+6] = ((ns/2)%2 == 1) ? '1' : '0' ;
   			ret [j+7] = (ns%2 == 1) ? '1' : '0' ;
   			seen[dir] = true ;
   		}
   	}

   	for (int i=0;i<8;i++){
   		if (!(seen[i])) {
   			int j = 8*i ;
   			ret [j+2] = ((i/4)%2 == 1) ? '1' : '0'  ;
   			ret [j+3] = ((i/2)%2 == 1) ? '1' : '0' ;
   			ret [j+4] = (i%2 == 1) ? '1' : '0' ;
   			ret [j] = '0' ;
   			ret [j+1] = '0' ;
   			ret [j+5] = '0' ;
   			ret [j+6] = '0' ;
   			ret [j+7] = '0' ;
   		}
   	}
    ret[64] = '\0';
   	return ret ;

}

char xor(char a, char b){
	if (a==b) return '0';
	else return '1';
}

char and(char a, char b){
	if (a=='1' && b=='1') return '1';
	else return '0';
}

char* decrypt(char C[32], char K[32]){
	char* P = malloc(33);
	int N0=0;
	for (int i=0;i<32;i++){if (K[i]=='0') N0++;}
	for (int i=0;i<32;i++) P[i]=C[i];
	P[32]='\0';

	char* T = malloc(5) ;
	T[4]='\0';
	T[3] = xor(K[31],xor(K[27],xor(K[23],xor(K[19],xor(K[15],xor(K[11],xor(K[7],K[3])))))));
	T[2] = xor(K[30],xor(K[26],xor(K[22],xor(K[18],xor(K[14],xor(K[10],xor(K[6],K[2])))))));
	T[1] = xor(K[29],xor(K[25],xor(K[21],xor(K[17],xor(K[13],xor(K[9],xor(K[5],K[1])))))));
	T[0] = xor(K[28],xor(K[24],xor(K[20],xor(K[16],xor(K[12],xor(K[8],xor(K[4],K[0])))))));

	// char* T8 = malloc(32);

	// T = T+15
	char* Carry = malloc(4);
	char* Sum = malloc(4);
	for (int l=0;l<4;l++) Carry[l]='0';
	Sum[0] = xor(T[0],'1');
	Carry[0] = and(T[0],'1');
	for (int c=1;c<4;c++){
		Sum[c] = xor(T[c],xor(Carry[c-1],'1'));
		Carry[c] = xor(and(Carry[c-1],T[c]),xor(and(Carry[c-1],'1'),and(T[c],'1')));
	}

	for (int k=0;k<4;k++) T[k]=Sum[k];

///
	for (int i=0;i<N0;i++){
		// for (int j=0;j<32;j++){T8[j] = T[j%4];}
		for (int j=0;j<32;j++){P[j]=xor(P[j],T[j%4]);}

		char* Carry = malloc(5);
		char* Sum = malloc(5);
		for (int l=0;l<4;l++) Carry[l]='0';
		Sum[0] = xor(T[0],'1');
		Carry[0] = and(T[0],'1');
		for (int c=1;c<4;c++){
			Sum[c] = xor(T[c],xor(Carry[c-1],'1'));
			Carry[c] = xor(and(Carry[c-1],T[c]),xor(and(Carry[c-1],'1'),and(T[c],'1')));
		}
		for (int k=0;k<4;k++) T[k]=Sum[k];

	}
	return P;
}

char* encrypt(char P[32], char K[32]){
	char* C = malloc(33);
	int N1=0;
	for (int i=0;i<32;i++){if (K[i]=='1') N1++;}
	for (int i=0;i<32;i++){C[i]=P[i];}
	char* T = malloc(5) ;
	C[32]='\0'; T[4]='\0';
	T[3] = xor(K[31],xor(K[27],xor(K[23],xor(K[19],xor(K[15],xor(K[11],xor(K[7],K[3])))))));
	T[2] = xor(K[30],xor(K[26],xor(K[22],xor(K[18],xor(K[14],xor(K[10],xor(K[6],K[2])))))));
	T[1] = xor(K[29],xor(K[25],xor(K[21],xor(K[17],xor(K[13],xor(K[9],xor(K[5],K[1])))))));
	T[0] = xor(K[28],xor(K[24],xor(K[20],xor(K[16],xor(K[12],xor(K[8],xor(K[4],K[0])))))));

	// char* T8 = malloc(33);

	for (int i=0;i<N1;i++){
		// for (int j=0;j<32;j++){T8[j] = T[j%4];}
		for (int j=0;j<32;j++){C[j]=xor(C[j],T[j%4]);}
		if(strcmp(T,"1111") == 1) T="0000";
		else {
			char* Carry = malloc(4);
			char* Sum = malloc(4);
			for (int l=0;l<4;l++) Carry[l]='0';
			Sum[0] = xor(T[0],'1');
			Carry[0] = and(T[0],'1');
			for (int c=1;c<4;c++){
				Sum[c] = xor(T[c],Carry[c-1]);
				Carry[c] = and(Carry[c-1],T[c]);
			}
			for(int k=0;k<4;k++) T[k]=Sum[k];

		}
	}
	return C;
}

char * read_byte(uint8 a){
		char* ans=malloc(9); 
		for(int i=0;i<8;i++) {
				if(a%2==0) { ans[i]= '0'; }
				else { ans[i]='1'; }
				a/=2; 
		}
		ans[8]='\0'; 
		return ans; 
}

char write_4byte_to_board(int chan, const char * data, struct FLContext *handle, const char * error , int timeout){
        char* to_enc = malloc(33);
        for(int i=0; i< 4; i++)
            for(int j= 0; j<8; j++)
                to_enc[i*8 +j] = data[i*8 + 7 - j];
        to_enc[32] = '\0';
		char *enc_data=encrypt(to_enc,key); 
		uint8 buf[5]; 
		uint8 temp=0; 
		for(int i=0;i<4;i++){
				temp=0; 
				for(int j=i<<3; j<(i<<3)+8; j++){ if(enc_data[j]=='1') { temp+=(1<<(j-(i<<3))); } }
				buf[i]=temp; 
		}
		FLStatus fStatus=flWriteChannel(handle,chan,4,buf,error); 
		if(fStatus!=FLP_SUCCESS) { 
				return 'f'; 
		}
		else return 's'; 
//
//		char *data_to_write=boolToHex(enc_data); 
//		strcpy(line1,"w1 "); 
//       // printf("%s\n",data_to_write); 
//		char *dd=malloc(3);  dd[2]='\0'; 
//		for(int i=0;i<4;i++){
//				dd[0]=data_to_write[i*2]; 
//				dd[1]=data_to_write[i*2+1]; 
//                strcpy(line1,"w1 "); 
//                strcat(line1,dd); 
//				pStatus = parseLine(handle, line1 , &error);
//				if(pStatus != FLP_SUCCESS) i--; 
//		}
//        wtt(); 
		
}
char * read_4byte_from_fpga(int chan, struct FLContext *handle, const char * error, int timeout ){
		char* data_red_from_fpga = malloc(33); 
		uint8 buf[5]; 
		FLStatus fstatus=flReadChannel(handle, chan, 2, buf, error); 
    	fstatus=flReadChannel(handle, chan, 4, buf, error); 
		if(fstatus!=FL_SUCCESS) { data_red_from_fpga[0]='f'; return data_red_from_fpga; }
		for(int i=0;i<4;i++){
				for(int j=0;j<8;j++){
						if((buf[i]&(1<<j)) > 0) { data_red_from_fpga[8*i+j]='1'; }
						else { data_red_from_fpga[8*i+j]='0'; }
				}
		}
		char* dec_data_from_fpga=decrypt(data_red_from_fpga,key); 
		return dec_data_from_fpga; 
   //     return data_red_from_fpga; 


//		char* line1="r0 1"; 
//		data_red_from_fpga[0]='\0'; 
//			   for(int i=0;i<4;i++){
//					   pStatus = parseLine(handle, line1, &error);
//					   if(pStatus == FLP_SUCCESS){
//                            //printf("read  %d th byte \n",i); 
//							   char *red_byte=read_byte(out); 
//							   strcat(data_red_from_fpga,red_byte); 
//					   }
//					   else {
//							   i--; 
//					   }
//			   }
//		data_red_from_fpga[32]='\0'; 
////		char* key=malloc(33); 
////
////        printf("%s\n", data_red_from_fpga); 
//        wtt(); 
//		return dec_data_from_fpga; 

}
char * read_1byte_from_fpgalink(int chan, struct FLContext *handle, const char * error, int timeout ){
		char* data_red_from_fpga = malloc(33); 
		uint8 buf[5];
        bool data_is_there=0; 
        while(timeout>0){
      //      printf("timeout in reverse order -- %d\n" , timeout); 
            sleep(0.05);
            timeout--;
            FLStatus fstatus = flReadChannel(handle,chan,1,buf,error); 
            if(buf[0]!=0) {
                data_is_there=1; 
                fstatus = flReadChannel(handle,chan,3,buf+1,error); 
                break; 
            }
        }
        if(!data_is_there) { data_red_from_fpga[0]='f';  return data_red_from_fpga; }
		for(int i=0;i<4;i++){
				for(int j=0;j<8;j++){
						if((buf[i]&(1<<j)) > 0) { data_red_from_fpga[8*i+j]='1'; }
						else { data_red_from_fpga[8*i+j]='0'; }
				}
		}
		char* dec_data_from_fpga=decrypt(data_red_from_fpga,key); 
		return dec_data_from_fpga; 
 
}


char* receive_ack(int chan, struct FLContext *handle, const char * error, int timeout) {
			char * data_red_from_fpga=read_4byte_from_fpga(chan,handle , error,timeout); 
            printf("received ack is %s\n",data_red_from_fpga);  
			if(data_red_from_fpga[0]=='f') return data_red_from_fpga; 
			for(int i=0;i<32;i++){
					if(data_red_from_fpga[i]!=ack1[i]) { data_red_from_fpga[0]='f';  return data_red_from_fpga; }
			}
            return data_red_from_fpga; 

}

char send_ack(int chan, struct FLContext *handle, const char * error, int timeout){
		char* byte_reversed = malloc(33);
        for(int i=0; i< 4; i++)
            for(int j= 0; j<8; j++)
                byte_reversed[i*8 +j] = ack2[i*8 + 7 - j];
        byte_reversed[32] = '\0';
		char write_status=write_4byte_to_board(chan,byte_reversed,handle,error,timeout); 
		return write_status; 

}

		

void wtt(){
//    int n=1000000000; 
//    int yyy=0; 
//    for(int i=0; i<n; i++) yyy++; 
//    for(int i=0; i<n; i++) yyy++; 
//    for(int i=0; i<n; i++) yyy++; 
//    for(int i=0; i<n; i++) yyy++; 
    usleep(100000); 
}
////////////////////////////////////////////////////////////////////////////////////////////////////
	
int main(int argc, char *argv[]) {
    
    fd = open (portname, O_RDWR | O_NOCTTY | O_SYNC);
	ReturnCode retVal = FLP_SUCCESS, pStatus;
	struct arg_str *ivpOpt = arg_str0("i", "ivp", "<VID:PID>", "            vendor ID and product ID (e.g 04B4:8613)");
	struct arg_str *vpOpt = arg_str1("v", "vp", "<VID:PID[:DID]>", "       VID, PID and opt. dev ID (e.g 1D50:602B:0001)");
	struct arg_str *rpOpt = arg_str0("r", "rp", "<File>", "       For running the program and giving the input file address");
	struct arg_str *fwOpt = arg_str0("f", "fw", "<firmware.hex>", "        firmware to RAM-load (or use std fw)");
	struct arg_str *portOpt = arg_str0("d", "ports", "<bitCfg[,bitCfg]*>", " read/write digital ports (e.g B13+,C1-,B2?)");
	struct arg_str *queryOpt = arg_str0("q", "query", "<jtagBits>", "         query the JTAG chain");
	struct arg_str *progOpt = arg_str0("p", "program", "<config>", "         program a device");
	struct arg_uint *conOpt = arg_uint0("c", "conduit", "<conduit>", "        which comm conduit to choose (default 0x01)");
	struct arg_str *actOpt = arg_str0("a", "action", "<actionString>", "    a series of CommFPGA actions");
	struct arg_lit *shellOpt  = arg_lit0("s", "shell", "                    start up an interactive CommFPGA session");
	struct arg_lit *benOpt  = arg_lit0("b", "benchmark", "                enable benchmarking & checksumming");
	struct arg_lit *rstOpt  = arg_lit0("r", "reset", "                    reset the bulk endpoints");
	struct arg_str *dumpOpt = arg_str0("l", "dumploop", "<ch:file.bin>", "   write data from channel ch to file");
	struct arg_lit *helpOpt  = arg_lit0("h", "help", "                     print this help and exit");
	struct arg_str *eepromOpt  = arg_str0(NULL, "eeprom", "<std|fw.hex|fw.iic>", "   write firmware to FX2's EEPROM (!!)");
	struct arg_str *backupOpt  = arg_str0(NULL, "backup", "<kbitSize:fw.iic>", "     backup FX2's EEPROM (e.g 128:fw.iic)\n");
	struct arg_end *endOpt   = arg_end(20);
	void *argTable[] = {
		ivpOpt, vpOpt, rpOpt, fwOpt, portOpt, queryOpt, progOpt, conOpt, actOpt,
		shellOpt, benOpt, rstOpt, dumpOpt, helpOpt, eepromOpt, backupOpt, endOpt
	};
	const char *progName = "flcli";
	int numErrors;
	struct FLContext *handle = NULL;
	FLStatus fStatus;
	const char *error = NULL;
	const char *ivp = NULL;
	const char *vp = NULL;
	bool isNeroCapable, isCommCapable;
	uint32 numDevices, scanChain[16], i;
	const char *line = NULL;
	uint8 conduit = 0x01;

	if ( arg_nullcheck(argTable) != 0 ) {
		fprintf(stderr, "%s: insufficient memory\n", progName);
		FAIL(1, cleanup);
	}

	numErrors = arg_parse(argc, argv, argTable);
/*
    if( rpOpt->count )
    {
        printf("Got the file %s\n", rpOpt->sval[0]);
		FAIL(FLP_SUCCESS, cleanup);
    }*/

	if ( helpOpt->count > 0 ) {
		printf("FPGALink Command-Line Interface Copyright (C) 2012-2014 Chris McClelland\n\nUsage: %s", progName);
		arg_print_syntax(stdout, argTable, "\n");
		printf("\nInteract with an FPGALink device.\n\n");
		arg_print_glossary(stdout, argTable,"  %-10s %s\n");
		FAIL(FLP_SUCCESS, cleanup);
	}

	if ( numErrors > 0 ) {
		arg_print_errors(stdout, endOpt, progName);
		fprintf(stderr, "Try '%s --help' for more information.\n", progName);
		FAIL(FLP_ARGS, cleanup);
	}

	fStatus = flInitialise(0, &error);
	CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);

	vp = vpOpt->sval[0];

	printf("Attempting to open connection to FPGALink device %s...\n", vp);
	fStatus = flOpen(vp, &handle, NULL);
	if ( fStatus ) {
		if ( ivpOpt->count ) {
			int count = 60;
			uint8 flag;
			ivp = ivpOpt->sval[0];
			printf("Loading firmware into %s...\n", ivp);
			if ( fwOpt->count ) {
				fStatus = flLoadCustomFirmware(ivp, fwOpt->sval[0], &error);
			} else {
				fStatus = flLoadStandardFirmware(ivp, vp, &error);
			}
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			
			printf("Awaiting renumeration");
			flSleep(1000);
			do {
				printf(".");
				fflush(stdout);
				fStatus = flIsDeviceAvailable(vp, &flag, &error);
				CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
				flSleep(250);
				count--;
			} while ( !flag && count );
			printf("\n");
			if ( !flag ) {
				fprintf(stderr, "FPGALink device did not renumerate properly as %s\n", vp);
				FAIL(FLP_LIBERR, cleanup);
			}

			printf("Attempting to open connection to FPGLink device %s again...\n", vp);
			fStatus = flOpen(vp, &handle, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
		} else {
			fprintf(stderr, "Could not open FPGALink device at %s and no initial VID:PID was supplied\n", vp);
			FAIL(FLP_ARGS, cleanup);
		}
	}

	printf(
		"Connected to FPGALink device %s (firmwareID: 0x%04X, firmwareVersion: 0x%08X)\n",
		vp, flGetFirmwareID(handle), flGetFirmwareVersion(handle)
	);

	if ( eepromOpt->count ) {
		if ( !strcmp("std", eepromOpt->sval[0]) ) {
			printf("Writing the standard FPGALink firmware to the FX2's EEPROM...\n");
			fStatus = flFlashStandardFirmware(handle, vp, &error);
		} else {
			printf("Writing custom FPGALink firmware from %s to the FX2's EEPROM...\n", eepromOpt->sval[0]);
			fStatus = flFlashCustomFirmware(handle, eepromOpt->sval[0], &error);
		}
		CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
	}

	if ( backupOpt->count ) {
		const char *fileName;
		const uint32 kbitSize = strtoul(backupOpt->sval[0], (char**)&fileName, 0);
		if ( *fileName != ':' ) {
			fprintf(stderr, "%s: invalid argument to option --backup=<kbitSize:fw.iic>\n", progName);
			FAIL(FLP_ARGS, cleanup);
		}
		fileName++;
		printf("Saving a backup of %d kbit from the FX2's EEPROM to %s...\n", kbitSize, fileName);
		fStatus = flSaveFirmware(handle, kbitSize, fileName, &error);
		CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
	}

	if ( rstOpt->count ) {
		// Reset the bulk endpoints (only needed in some virtualised environments)
		fStatus = flResetToggle(handle, &error);
		CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
	}

	if ( conOpt->count ) {
		conduit = (uint8)conOpt->ival[0];
	}

	isNeroCapable = flIsNeroCapable(handle);
	isCommCapable = flIsCommCapable(handle, conduit);

	if ( portOpt->count ) {
		uint32 readState;
		char hex[9];
		const uint8 *p = (const uint8 *)hex;
		printf("Configuring ports...\n");
		fStatus = flMultiBitPortAccess(handle, portOpt->sval[0], &readState, &error);
		CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
		sprintf(hex, "%08X", readState);
		printf("Readback:   28   24   20   16    12    8    4    0\n          %s", nibbles[*p++ - '0']);
		printf(" %s", nibbles[*p++ - '0']);
		printf(" %s", nibbles[*p++ - '0']);
		printf(" %s", nibbles[*p++ - '0']);
		printf("  %s", nibbles[*p++ - '0']);
		printf(" %s", nibbles[*p++ - '0']);
		printf(" %s", nibbles[*p++ - '0']);
		printf(" %s\n", nibbles[*p++ - '0']);
		flSleep(100);
	}

	if ( queryOpt->count ) {
		if ( isNeroCapable ) {
			fStatus = flSelectConduit(handle, 0x00, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			fStatus = jtagScanChain(handle, queryOpt->sval[0], &numDevices, scanChain, 16, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			if ( numDevices ) {
				printf("The FPGALink device at %s scanned its JTAG chain, yielding:\n", vp);
				for ( i = 0; i < numDevices; i++ ) {
					printf("  0x%08X\n", scanChain[i]);
				}
			} else {
				printf("The FPGALink device at %s scanned its JTAG chain but did not find any attached devices\n", vp);
			}
		} else {
			fprintf(stderr, "JTAG chain scan requested but FPGALink device at %s does not support NeroProg\n", vp);
			FAIL(FLP_ARGS, cleanup);
		}
	}

	if ( progOpt->count ) {
		printf("Programming device...\n");
		if ( isNeroCapable ) {
			fStatus = flSelectConduit(handle, 0x00, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			fStatus = flProgram(handle, progOpt->sval[0], NULL, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
		} else {
			fprintf(stderr, "Program operation requested but device at %s does not support NeroProg\n", vp);
			FAIL(FLP_ARGS, cleanup);
		}
	}

	if ( benOpt->count ) {
		enableBenchmarking = true;
	}
	
	if ( actOpt->count ) {
		printf("Executing CommFPGA actions on FPGALink device %s...\n", vp);
		if ( isCommCapable ) {
			uint8 isRunning;
			fStatus = flSelectConduit(handle, conduit, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			fStatus = flIsFPGARunning(handle, &isRunning, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			if ( isRunning ) {
				pStatus = parseLine(handle, actOpt->sval[0], &error);
				CHECK_STATUS(pStatus, pStatus, cleanup);
			} else {
				fprintf(stderr, "The FPGALink device at %s is not ready to talk - did you forget --program?\n", vp);
				FAIL(FLP_ARGS, cleanup);
			}
		} else {
			fprintf(stderr, "Action requested but device at %s does not support CommFPGA\n", vp);
			FAIL(FLP_ARGS, cleanup);
		}
	}

	if ( dumpOpt->count ) {
		const char *fileName;
		unsigned long chan = strtoul(dumpOpt->sval[0], (char**)&fileName, 10);
		FILE *file = NULL;
		const uint8 *recvData;
		uint32 actualLength;
		if ( *fileName != ':' ) {
			fprintf(stderr, "%s: invalid argument to option -l|--dumploop=<ch:file.bin>\n", progName);
			FAIL(FLP_ARGS, cleanup);
		}
		fileName++;
		printf("Copying from channel %lu to %s", chan, fileName);
		file = fopen(fileName, "wb");
		CHECK_STATUS(!file, FLP_CANNOT_SAVE, cleanup);
		sigRegisterHandler();
		fStatus = flSelectConduit(handle, conduit, &error);
		CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
		fStatus = flReadChannelAsyncSubmit(handle, (uint8)chan, 22528, NULL, &error);
		CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
		do {
			fStatus = flReadChannelAsyncSubmit(handle, (uint8)chan, 22528, NULL, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			fStatus = flReadChannelAsyncAwait(handle, &recvData, &actualLength, &actualLength, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			fwrite(recvData, 1, actualLength, file);
			printf(".");
		} while ( !sigIsRaised() );
		printf("\nCaught SIGINT, quitting...\n");
		fStatus = flReadChannelAsyncAwait(handle, &recvData, &actualLength, &actualLength, &error);
		CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
		fwrite(recvData, 1, actualLength, file);
		fclose(file);
	}
//   cccccccccccccccccccccccccccccc
    if( rpOpt->count )
    {
        //printf("Got the file %s\n", rpOpt->sval[0]);
    
        printf(CC "==========================================================================================================\n" RST);
        printf(CM "=============================== " CR "DLD PROJECT BY THE XILINX MEN" CM " ============================================\n" RST);
        printf(CC "==========================================================================================================\n" RST);
        
		entire_process:
		
        printf("\nReading Track Data From the CSV File...\n");
            
   		int table[640][5];
        int rows;
		const char* path = rpOpt->sval[0];
		rows = read_table(path, table);
	//	for(int i=0; i<rows; i++)
	//	{
	//		printf("%d, %d, %d, %d, %d\n", table[i][0], table[i][1], table[i][2], table[i][3], table[i][4]);
	//	}

        printf(CG "Track Data Read\n" RST);   
 
    	sleep(1); 
        
        int x_coordinate[65],y_coordinate[65]; 
    
    	host_label_2:

    	for(int i=0;i<65;i++) { x_coordinate[i]=-1; y_coordinate[i]=-1; }
    	
        int start_i=0, end_i=64; 
        
        printf(CB "\nStarting Polling Process\n");  	

        for(int i=start_i;i<end_i;i++){
    			if(isCommCapable){
    					uint8 isRunning; 
    					fStatus=flIsFPGARunning(handle,&isRunning,&error);
    					CHECK_STATUS(fStatus,FLP_LIBERR,cleanup); 
    					if(isRunning){
    							printf("\n>> Attempting to read Co-ordinates from Channel " CM "%d\n" RST,2*i);

    							char* red_data=read_4byte_from_fpga(2*i,handle,error,32); 

    							if(red_data[0]=='f') { printf(CR ">>>> Failed to Read co-ordinates from channel\n" RST); continue; }

                                printf(CG ">>>> Successfully Read co-ordinates from channel "  " %d\n" RST,2*i);

                                printf(">>>>>>>> Read data is " CB " %s\n" RST,red_data); 

                                printf(">> Attempting to Write coordinates at channel " CM "%d\n" RST,2*i+1); 

    							char write_status=write_4byte_to_board(2*i+1,red_data,handle,error,32); 

    							if(write_status=='f') { printf(CR "Write failed, Moving on to the next channel\n" RST); continue; }
                                
                                printf(CG ">>>> Successfully Written Co-ordinates to the channel\n" RST);
        
                                printf(">> Attempting to receive ACK1\n"); 

    							char* ack_status=receive_ack(2*i,handle,error,1); 

    							if(ack_status[0]=='f'){

                                        printf(CR ">>>> First ack was incorrect, re-attempt after 5 seconds\n" RST); 
    									sleep(5); 
                                        printf(">> Attempting to receive ACK1 for the second time\n"); 
    									ack_status=receive_ack(2*i,handle,error,1); 
    									if(ack_status[0]='f'){
                                                printf(CR ">>>> Second ACK was also incorrect, Moving on to the next channel\n" RST); 
    											/*start_i=(i+1)%64; 
    											end_i=i+1; 
    											i=start_i-1; 
    											*/
    											continue; 
    									}
    							}
                                
                                printf(CG ">>>> Successfully received ACK1 and co-ordinates\n" RST);
    							get_coordinate(red_data,&x_coordinate[i],&y_coordinate[i]); 
                                break; 
    					}
    			}
    	}

    	int read_chan, write_chan; for(int i=0;i<64;i++) { if(x_coordinate[i]!=-1) { read_chan=2*i;write_chan=read_chan+1;  break; } } 

        printf(">> Co-ordinates are " CB "(%d,%d)\n" RST,x_coordinate[read_chan>>1],y_coordinate[read_chan>>1]); 
        
        printf(">> Sending ACK2 to the board\n");
    	send_ack(write_chan,handle, error,32); 

    	char* track_data=func(x_coordinate[read_chan>>1],y_coordinate[read_chan>>1],table,rows); 
    
        printf(">> Writing 64 bits to board\n"); 
        printf(">>>> Data to write to the board is " CM "%s\n" RST, track_data);
       
    	write_4byte_to_board(write_chan,track_data,handle, error,32); 
        printf(CG ">>>> Written first four bytes to board\n" RST);
        printf(">>>> Waiting to receive ACK\n");
    	char* ack_status=receive_ack(read_chan,handle, error, 256); 
    	if(ack_status[0]=='f') { 
                // TODO host label 2
                printf(CR ">>>> Receiving Ack failed, going to host_label_2\n");
                goto host_label_2; }
    	else {
                
    			write_4byte_to_board(write_chan,track_data+32,handle,error,32); 
                printf(CG ">>>> Written next four bytes to board\n" RST);
                printf(">>>> Waiting to receive ACK\n");
    			ack_status=receive_ack(read_chan,handle, error, 256); 
    			if(ack_status[0]=='f') { 
                            printf(CR ">>>> Receiving Ack failed, going to host_label_2\n");
                            goto host_label_2; 
                    }
    			else {
                        
    				send_ack(write_chan,handle,error,32); 
    				printf(CG ">>>> Sent ACK\n" RST);
                    // goto entire_process; 
    				// Read four bytes
    				// Wait until timeout

                    printf("Sleeping until the display of the board is over\n");
                    sleep(22);
        
                    clock_t start, stop;
                    start = clock();

                   	int flag1 = 0 , flag2 = 0;
                    
                    int timeout_s = 20;
                    
                    printf("Waiting to receive data from the board via FPGA and/or UART\n");    

                    while (100*((double)(clock())-start)/CLOCKS_PER_SEC < timeout_s) {
                        //printf("%.9f sec\n",100*((double)(clock())-start)/CLOCKS_PER_SEC);  
                        //printf("%.8f sec\n", clock()-start);
                        if (!(flag1)){
                              // 32 tries
                              char* red_data=read_1byte_from_fpgalink(read_chan,handle,error,32);    				
                              if(red_data[0]=='f') {// printf("data for track update was not available at fpgalink \n"); 
                                                }
                              else {
                                  printf(CG ">> Read track data from FPGA\n" RST); 
                                  printf(">>>> Read data is " CM "%s\n" RST,red_data);
                                  flag1 = 1 ;
        	          			// change the entry corresponding to these bytes in the table
    
        	          			int x_c = x_coordinate[read_chan>>1];
        	          			int y_c = y_coordinate[read_chan>>1];
    
        	          			int x,y,z ;
        	          			x = (red_data[7] == '1') ? 1 : 0  ;
        	          			y = (red_data[6] == '1') ? 1 : 0  ;
        	          			z = (red_data[5] == '1') ? 1 : 0  ;
    
        	          			int dir = x*4 + y*2 + z;
    
        	          			for (int count =0 ; count < rows ; count++){
    
        	          				if (table[count][0] == x_c && table[count][1]==y_c && table[count][2]==dir) {
        	          					table[count][3] = red_data[3]-48;
    
       		          					x = (red_data[2] == '1') ? 1 : 0  ;
       		          					y = (red_data[1] == '1') ? 1 : 0  ;
       		          					z = (red_data[0] == '1') ? 1 : 0  ;
    
        	          					table[count][4] = x*4 + y*2 + z;
        	          					break;
        	          				}
        	          			}
                                  printf(">>>> Changed the entry to the table\n");
        	          			// Save table
        //	          			save_to_file(table,rows);
    
                                  printf(">>>> Saved the table to the file\n");
                              }
                        }
        				// Read data from UART port
        			//	if (!(flag2)) {
            		//		char* red_uart=read_one_byte_from_uart(0.05);
                    //       // }                                                                 
                    //        if(red_uart[0] == 'f' && !(flag2)) { 
                    //          //  printf("Data for track update is not available at uart port \n");
                    //        }
                    //        else {
                    //            printf(CG ">> Read data Successfully via UART\n" RST);
                    //           	printf(">>>> Data Received is" CB "%s\n" RST, red_uart);
                    //            flag2 = 1 ;
                    //        }
                    //    }
                    if(flag1 == 1 ) break;// && flag2 == 1) break;
                    }

                    if(!(flag1)) printf (">> No data received from controller via FPGA\n"); 
                   // if(!(flag2)) printf (">> No data received from controller via UART\n"); 
                    flag1 = 0 ;
                	flag2 = 0 ;
                   	// TODO To write before this step
                   	// Make the controller to wait?
/*
                    printf("Writing Data to UART\n");
    	    		if (write_one_byte_to_uart() == 0){
    	    			printf(CR ">> Writing to UART failed\n" RST);
					printf(CB "Starting the Entire Process\n" RST);
    	    			goto host_label_2;
    	    		}
    	    		printf(CG ">> Write to UART Successful\n" RST);

    	    		printf(CB "Starting the Entire Process\n" RST);*/
                    sleep(30);
                    goto host_label_2; 
    			}

	}

//
//    while(true){
//		if ( isCommCapable ) {
//		   uint8 isRunning;
//			fStatus = flSelectConduit(handle, conduit, &error);
//			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
//			fStatus = flIsFPGARunning(handle, &isRunning, &error);
//			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
//            //printf("is commCapable\n"); 
//            if ( isRunning ) {
//               printf("is Running \n"); 
//			   char* red_data=read_4byte_from_fpga(handle, error);
//               printf("Red coordinates  %s\n", red_data); 
//			   write_4byte_to_board(red_data, handle, error); 
//               printf("wrote coordinates %s\n", red_data); 
//			   //red_data=read_4byte_from_fpga(handle, error); 
//               receive_ack(handle,error); 
//               printf("received ack1 for corordinates\n"); 
//			   send_ack(handle, error); 
//               printf("sent ack2 from host to board \n"); 
//    	       char* writeData = func(red_data, table, rows);
//               printf("Data to be sent to board %s\n",writeData); 
//			   write_4byte_to_board(writeData, handle, error); 
//               printf("wrote first 32 bit of track data \n"); 
//			   //read_4byte_from_fpga(handle, error); 
//               receive_ack(handle,error); 
//               printf("received ack1 \n"); 
//			   write_4byte_to_board(&writeData[32], handle, error); 
//               printf("wrote last 32 bit of track data\n"); 
//			   //read_4byte_from_fpga(handle, error); 
//               receive_ack(handle,error); 
//               printf("received ack1\n"); 
//			   send_ack(handle, error); 
//               printf("sent ack2\n"); 
//               printf(" completed one sequence of communication \n\n\n\n");
//            }
//        	}
//    	}
	}
	if ( shellOpt->count ) {
		printf("\nEntering CommFPGA command-line mode:\n");
		if ( isCommCapable ) {
		   uint8 isRunning;
			fStatus = flSelectConduit(handle, conduit, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			fStatus = flIsFPGARunning(handle, &isRunning, &error);
			CHECK_STATUS(fStatus, FLP_LIBERR, cleanup);
			if ( isRunning ) {
				do {
					do {
						line = readline("> ");
					} while ( line && !line[0] );
					if ( line && line[0] && line[0] != 'q' ) {
						add_history(line);
						pStatus = parseLine(handle, line, &error);
						CHECK_STATUS(pStatus, pStatus, cleanup);
						free((void*)line);
					}
				} while ( line && line[0] != 'q' );
			} else {
				fprintf(stderr, "The FPGALink device at %s is not ready to talk - did you forget --xsvf?\n", vp);
				FAIL(FLP_ARGS, cleanup);
			}
		} else {
			fprintf(stderr, "Shell requested but device at %s does not support CommFPGA\n", vp);
			FAIL(FLP_ARGS, cleanup);
		}
	}

cleanup:
	free((void*)line);
	flClose(handle);
	if ( error ) {
		fprintf(stderr, "%s\n", error);
		flFreeError(error);
	}
	return retVal;
}
