# Auto Issuing a Reply with the Reply ID to An MVS Console Message through an MPF LIST exit

This repository demonstrates how to write an MVS exit in Assembler to automatically reply to a console message through the setup of an MPF List member (MPFLST00)in SYS1.PARMLIB.

In this sample, the RACF messages: **ICH302D** -- caused by maximum password attempts by a SPECIAL id logon) and **ICH304D** -- caused by a SPECIAL id logon when the Inactive Interval Exceeds --  are intercepted to trigger the running of this exit to automatically reply a 'Y'.   

1. **MPFLST00.txt** shows how to set up MPF list.

2. The program: **ICH3024D.asm** must be compiled and link-edited into an LMD in one of the LNKLST data sets -- **SYS1.USER.LINKLIB** in this sample. Use *** D PROG,LNKLST *** to display the Link List concatenation.  

3. In **ICH3024D.asm**, the job name of the address space is checked to see if the logon is from a address space named as 'CICS????'. the program will go on only if the answer is YES and do nothing otherwise. You may change it to anything else or get rid of the checking totally. 

4. Replace **CONSOLE1** with the a console name defined in RACF and CONSOLxx in SYS1.PARMLIB -- the adequate console ID that has the authority to issue a 'R xx,...' command.

5. Once the LMD of ICH3024D generated, and the MPFLST00 (or MPFLSTxx, if a different version: 'xx' of MPFLST is used) is updated, issue **'F LLA,REFRESH'** then **'T MPF=00'** (or other version).

6. Upload in binary mode ANDREWJ.SOURCE.MAC#.xmit to an LRECL=80 sequential file then receive it to a PDS for macros needed in **ICH3024D.asm**.