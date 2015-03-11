/*==========================================================================
  author Michal Slaski t: @michalslaski

  NFC library provides functions for reading and writing mifare chips.
  More details http://wiki.iteadstudio.com/ITEAD_PN532_NFC_MODULE
 ===========================================================================*/

#include "nfc.h"

void main(void) 
{
  uint32_t versiondata;
  uint32_t id, prev_id = 0;

  // Begin to communicate with the ITEAD NFC module
  begin();

  // Get the firmware version of the NFC module
  versiondata = getFirmwareVersion();
  if (! versiondata) {
    fprintf(stderr, "ERROR: No PN53x board found\n");
    return;
  }

  // configure board to read RFID tags and cards
  SAMConfig();
	
  while(1) {
    // Get the passive target card ID
    id = readPassiveTargetID(PN532_MIFARE_ISO14443A);

    if (id != prev_id && id != 0) {
      fprintf(stdout, "%d\n", id);
      fflush(stdout);
      prev_id = id;
    }
  }
}


