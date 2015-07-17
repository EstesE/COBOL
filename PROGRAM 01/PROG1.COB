       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROG1.
      *AUTHOR.        ERIC.
      *INSTALLATION.  WSC.
      *DATE WRITTEN.  SEPT 20,2002.
      *SECURITY.      NONE.




       ENVIRONMENT DIVISION.



       INPUT-OUTPUT SECTION.


       FILE-CONTROL.
           SELECT EMPL-FILE-IN
               ASSIGN TO COBOLDATA2D.
           SELECT EMPL-REPORT-OUT 
               ASSIGN TO PROG1-OUT.


           
       DATA DIVISION.


       FILE SECTION.


       FD  EMPL-FILE-IN
               RECORD CONTAINS 80 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS ER-EMPL-REC-IN.


       01  ER-EMPL-REC-IN.
           05  ER-EMPL-DEPT-NUM-IN                 PIC 9(2).
           05  ER-SSN-IN.
               10  ER-SSN-1ST-3-CHAR-IN            PIC 9(3).
               10  ER-SSN-MID-2-CHAR-IN            PIC 9(2).
               10  ER-SSN-LST-4-CHAR-IN            PIC 9(4).
           05  ER-FULL-NAME-IN.
               10  ER-LAST-NAME-IN.
                   15  ER-1ST-CHAR-IN              PIC X(1).
                   15  ER-REST-OF-LNAME-IN         PIC X(11).
               10  ER-FIRST-NAME-IN                PIC X(11).
               10  ER-MID-INIT-IN                  PIC X(1).
           05  ER-ADDRESS-IN                       PIC X(15).
           05  FILLER                              PIC X(10).
           05  ER-CITY-IN                          PIC X(13).
           05  ER-PHONE-NUM-IN.
               10  ER-PHONE-PREFIX-IN              PIC 9(3).
               10  ER-PHONE-LINE-IN                PIC 9(4).
          
       FD  EMPL-REPORT-OUT
               RECORD CONTAINS 132 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS EL-EMPL-LINE-OUT.


       01  EL-EMPL-LINE-OUT.
           05  EL-FULL-NAME-OUT.
               10  EL-LAST-NAME-OUT                PIC X(12).
               10  FILLER                          PIC X(2).
               10  EL-FIRST-NAME-OUT               PIC X(11).
               10  FILLER                          PIC X(1).
               10  EL-MID-INIT-OUT                 PIC X(1).
           05  FILLER                              PIC X(2).
           05  EL-SSN-OUT.
               10  EL-SSN-1ST-3-CHAR-OUT           PIC 9(3).
               10  EL-HYPHEN-1-OUT                 PIC X(1).
               10  EL-SSN-MID-2-CHAR-OUT           PIC 9(2).
               10  EL-HYPHEN-2-OUT                 PIC X(1).
               10  EL-SSN-LST-4-CHAR-OUT           PIC 9(4).
           05  FILLER                              PIC X(2).
           05  EL-ADDRESS-OUT                      PIC X(15).
           05  FILLER                              PIC X(12).
           05  EL-CITY-OUT                         PIC X(13).
           05  FILLER                              PIC X(2).
           05  EL-PHONE-NUM-OUT.
               10  EL-PHONE-PREFIX-OUT             PIC 9(3).
               10  EL-HYPHEN-3-OUT                 PIC X(1).
               10  EL-PHONE-LINE-OUT               PIC 9(4).
           05  FILLER                              PIC X(3).
           05  EL-EMPL-DEPT-NUM-OUT                PIC 9(2).
           05  FILLER                              PIC X(4).
           05  EL-EMPL-CODE-OUT.
               10  EL-EMPL-CODE-1ST-CHAR-OUT       PIC X(1).
               10  EL-EMPL-CODE-NEXT4-CHAR-OUT     PIC 9(4).
               10  EL-EMPL-CODE-LAST2-CHAR-OUT     PIC 9(2).
           05  FILLER                              PIC X(24).



       WORKING-STORAGE SECTION.


       01  WS-PROGRAM-SWITCHES.
           05 WS-EOF-SWITCH                        PIC X(3).




       PROCEDURE DIVISION.



       A00-MAINLINE-PARA.


           OPEN INPUT EMPL-FILE-IN
               OUTPUT EMPL-REPORT-OUT.
           PERFORM B10-INIT-PARA.
           READ EMPL-FILE-IN
               AT END MOVE "YES" TO WS-EOF-SWITCH.
           PERFORM B20-PROCESS-PARA
               UNTIL WS-EOF-SWITCH IS EQUAL TO "YES".
           CLOSE EMPL-FILE-IN
                 EMPL-REPORT-OUT.
           STOP RUN.



       B10-INIT-PARA.
           MOVE "NO" TO WS-EOF-SWITCH.


       B20-PROCESS-PARA.
           MOVE SPACES TO EL-EMPL-LINE-OUT.
           MOVE ER-LAST-NAME-IN TO EL-LAST-NAME-OUT.
           MOVE ER-FIRST-NAME-IN TO EL-FIRST-NAME-OUT.
           MOVE ER-MID-INIT-IN TO EL-MID-INIT-OUT.
           MOVE ER-SSN-1ST-3-CHAR-IN TO EL-SSN-1ST-3-CHAR-OUT.
           MOVE "-" TO EL-HYPHEN-1-OUT.
           MOVE ER-SSN-MID-2-CHAR-IN TO EL-SSN-MID-2-CHAR-OUT.
           MOVE "-" TO EL-HYPHEN-2-OUT.
           MOVE ER-SSN-LST-4-CHAR-IN TO EL-SSN-LST-4-CHAR-OUT.
           MOVE ER-ADDRESS-IN TO EL-ADDRESS-OUT.
           MOVE ER-CITY-IN TO EL-CITY-OUT.
           MOVE ER-PHONE-PREFIX-IN TO EL-PHONE-PREFIX-OUT.
           MOVE "-" TO EL-HYPHEN-3-OUT.
           MOVE ER-PHONE-LINE-IN TO EL-PHONE-LINE-OUT.
           MOVE ER-EMPL-DEPT-NUM-IN TO EL-EMPL-DEPT-NUM-OUT.
           MOVE ER-1ST-CHAR-IN TO EL-EMPL-CODE-1ST-CHAR-OUT.
           MOVE ER-SSN-LST-4-CHAR-IN TO EL-EMPL-CODE-NEXT4-CHAR-OUT.
           MOVE ER-EMPL-DEPT-NUM-IN TO EL-EMPL-CODE-LAST2-CHAR-OUT.
           WRITE EL-EMPL-LINE-OUT
               AFTER ADVANCING 2 LINES.
           READ EMPL-FILE-IN
               AT END MOVE "YES" TO WS-EOF-SWITCH. 