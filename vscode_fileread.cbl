       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-FILE-LOCAL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "read_input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "write_output.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE.

       01 INRECORD.
           05 CITYKEY  PIC A(5).
           05 CITYNAME PIC A(12).
           05 CITYTYPE PIC A(5).
           05 CITYCODE PIC 9(6).
           05 FAKE-CLM-FROM-DT-X           PIC X(1).
           05 FAKE-CLM-FROM-DT             PIC S9(5) COMP-3.
           05 FAKE-CLM-FROM-DT-1           PIC S9(5) COMP-3.
           05 FAKE-CLM-FROM-INT            PIC 9(7).
           05 FAKE-CLM-FROM-BINARY         PIC 9 COMP.

       FD OUTFILE.

       01 OUTRECORD.
           05 CITYKEY-OUTPUT  PIC A(5).
           05 CITYNAME-OUTPUT PIC A(12).
           05 CITYTYPE-OUTPUT PIC A(5).
           05 CITYCODE-OUTPUT PIC 9(6).
           05 FAKE-CLM-FROM-DT-X-OUTPUT           PIC X(1).
           05 FAKE-CLM-FROM-DT-OUTPUT             PIC S9(5) COMP-3.
           05 FAKE-CLM-FROM-DT-1-OUTPUT           PIC S9(5) COMP-3.
           05 FAKE-CLM-FROM-INT-OUTPUT            PIC 9(7).
           05 FAKE-CLM-FROM-BINARY-OUT            PIC 9 COMP.


       WORKING-STORAGE SECTION.

    *>    COPY "HGCPCLM.cpy".
       
       01 SWITCHES.
           05 EOF-SWITCH PIC X VALUE "N".

       01 COUNTERS.
           05 REC-COUNTER PIC 9(3) VALUE 0.

       01 W-STORE-PD PIC S9(5) COMP-3 VALUE -0. 

       LOCAL-STORAGE SECTION.

       01 L-STORE-PD PIC S9(5) COMP-3 VALUE +72001. 

       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-INITIALIZE.
           PERFORM 200-PROCESS-RECORDS
               UNTIL EOF-SWITCH = "Y".
           PERFORM 300-TERMINATE.
           STOP RUN.

       100-INITIALIZE.
           OPEN INPUT INFILE
                OUTPUT OUTFILE.

       200-PROCESS-RECORDS.
           READ INFILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END
                   MOVE CITYKEY TO CITYKEY-OUTPUT
                   MOVE CITYNAME TO CITYNAME-OUTPUT
                   MOVE CITYTYPE TO CITYTYPE-OUTPUT
                   MOVE CITYCODE TO CITYCODE-OUTPUT
                   MOVE FAKE-CLM-FROM-DT-X TO FAKE-CLM-FROM-DT-X-OUTPUT
                   MOVE FAKE-CLM-FROM-DT TO FAKE-CLM-FROM-DT-OUTPUT
                   MOVE FAKE-CLM-FROM-DT-1 TO FAKE-CLM-FROM-DT-1-OUTPUT
                   MOVE FAKE-CLM-FROM-INT TO FAKE-CLM-FROM-INT-OUTPUT
                   MOVE FAKE-CLM-FROM-BINARY TO FAKE-CLM-FROM-BINARY-OUT
                   COMPUTE REC-COUNTER = REC-COUNTER + 1
           END-READ.
           
           IF EOF-SWITCH = "N" THEN
               DISPLAY "KEY  >>>> " CITYKEY
               DISPLAY "NAME >>>> " CITYNAME
               DISPLAY "TYPE >>>> " CITYTYPE
               DISPLAY "CODE >>>> " CITYCODE
               DISPLAY "ALL >>>> " OUTRECORD
               DISPLAY "FAKE-CLM-FROM-DT-X >>> " FAKE-CLM-FROM-DT-X
               DISPLAY "FAKE-CLM-FROM-DT >>> " FAKE-CLM-FROM-DT
               DISPLAY "FAKE-CLM-FROM-DT-1 >>> " FAKE-CLM-FROM-DT-1
               DISPLAY "FAKE-CLM-FROM-INT >>> " FAKE-CLM-FROM-INT
               DISPLAY "FAKE-CLM-FROM-BINARY >>> " FAKE-CLM-FROM-BINARY

               WRITE OUTRECORD
           END-IF.

       300-TERMINATE.
           DISPLAY "NUM OF RECS >>>> " REC-COUNTER.
           DISPLAY "W-STORE-PD >>> " W-STORE-PD.
           DISPLAY "L-STORE-PD >>> " L-STORE-PD.
           CLOSE INFILE
                 OUTFILE.

       END PROGRAM READ-FILE-LOCAL.
