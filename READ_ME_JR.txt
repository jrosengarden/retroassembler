    Updated: 08/08/22 (JR) MAC NOTES

NOTE:
Use the following commands to see the RetroAssembler (Visual Studio Code) Commands:

1.  Select:		/file/preferences/keyboard shortcuts
2.  Then:		type "retro" in the search box

NOTE1:
All documentation is in the DOCUMENTATON folder

NOTE2:
1.	The executable (retroassembler) is located at: \\Mac\Dropbox\Jeff\Assembly Programming\retroassembler v2022.1
2.	The executable has been added into the system PATH environment variable so it can be called from anywhere (WINDOWS ONLY)
	OSX NOTE:  in order to use retroassembler from anywhere, after dropping into terminal, use the command retro
			   - The alias of "retro" has been setup in .zshrc (OSX ONLY)
3.	The Visual Studio Code RetroAssembler setting for the retroassembler path simply has "retroassembler" entered
	into the field due to #2 above (WINDOWS ONLY)
3a. The Visual Studio Code RetroAssembler setting for the retroassembler path has been set as follows:
	dotnet "/users/jeffrosengarden/dropbox/jeff/assembly programming/retroassembler v2022.1/retroassembler.dll"
	(OSX ONLY)
4.	Add the following to any 6502 assembly code if you plan to use it with the AppleWin emulator (using the paste feature):
	// the following .format and .setting's allow the generated output
	// file to be directly pasted into the AppleWin emulator
	.format "txt"				; set output file format as ASCII TXT file 
	.setting "OutputTxtAddressFormatFirst","{0:x04}: "
	.setting "OutputTxtAddressFormatNext","{0:x04}: "
5.	See \\Mac\Dropbox\Jeff\Assembly Programming\retroassembler v2022.1\Examples\6502\add.asm for a complete example
	that assembles properly so that the output file add.txt can be directly copied then pasted into the AppleWin Emulator

NOTE3:
While the executable is located in Dropbox (see above):
	--when working from Windows (This virtual computer) use theRetroAssembler v2022.1 folder for all work.
	--when working from Mac (by dropping into terminal from the DropBox RetroAssembler v2022.1 folder) use the dropbox
        location for all work

