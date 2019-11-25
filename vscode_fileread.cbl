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
           05 FAKE-CLM-FROM-DT-X           PIC X(01).
           05 FAKE-CLM-FROM-DT             PIC S9(05) COMP-3.

       FD OUTFILE.

       01 OUTRECORD.
           05 CITYKEY-OUTPUT  PIC A(5).
           05 CITYNAME-OUTPUT PIC A(12).
           05 CITYTYPE-OUTPUT PIC A(5).
           05 CITYCODE-OUTPUT PIC 9(6).
           05 FAKE-CLM-FROM-DT-X-OUTPUT           PIC X(01).
           05 FAKE-CLM-FROM-DT-OUTPUT             PIC S9(05) COMP-3.

       WORKING-STORAGE SECTION.
       
       01 SWITCHES.
           05 EOF-SWITCH PIC X VALUE "N".

       01 COUNTERS.
           05 REC-COUNTER PIC 9(3) VALUE 0.

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
                   COMPUTE REC-COUNTER = REC-COUNTER + 1
           END-READ.
           
           IF EOF-SWITCH = "N" THEN
               DISPLAY "KEY  >>>> " CITYKEY
               DISPLAY "NAME >>>> " CITYNAME
               DISPLAY "TYPE >>>> " CITYTYPE
               DISPLAY "CODE >>> > " CITYCODE
               DISPLAY "ALL >>>> " OUTRECORD
               DISPLAY "FAKE-CLM-FROM-DT-X >>> " FAKE-CLM-FROM-DT-X
               DISPLAY "FAKE-CLM-FROM-DT >>> " FAKE-CLM-FROM-DT

               WRITE OUTRECORD
           END-IF.

       300-TERMINATE.
           DISPLAY "NUM OF RECS >>>> " REC-COUNTER.
           CLOSE INFILE
                 OUTFILE.

       END PROGRAM READ-FILE-LOCAL.
