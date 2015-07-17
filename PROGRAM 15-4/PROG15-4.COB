  IDENTIFICATION DIVISION.
      *
      *
       PROGRAM-ID.    PROG15-4.
      *AUTHOR.        ERIC.
      *INSTALLATION.  WSC.
      *DATE-WRITTEN.  APR 14,2003.
      *DATE-COMPILED. APR 14,2003.
      *SECURITY.      NONE.
      *
      *
      *
      * PROGRAM OPERATIONS:
      * 
      * 1  CREATES AN INDEXED EQUIVALENT OF THE STUDENT MASTER FILE
      *    (STUDENT-NUMBER IS THE KEY FIELD).
      * 2  CREATES AN INDEXED EQUIVALENT OF THE GRADE FILE
      *    (STUDENT-NUMBER + RECORD COUNT FOR PRIMARY KEY AND
      *    STUDENT-NUMBER AS THE SECONDARY KEY).
      * 3  DISPLAY AN ANNOUNCEMENT SCREEN.
      * 4  QUERY THE USER FOR THE DESIRED STUDENT NUMBER.
      * 5  READ THE STUDENT MASTER FROM DISK.
      *    A  IF NO MASTER FOR THE REQUEST, DISPLAY AN ERROR MESSAGE.
      *    B  DISPLAY STUDENT MASTER DATA; CALCULATE GRADE POINT
      *       AVERAGE = EARNED GRADE POINTS/TOTAL UNITS.
      * 6  READ EACH COURSE RECORD FOR THIS STUDENT.
      * 7  IF THE REQUESTED STUDENT HAS NO COURSE RECORDS, DISPLAY AN
      *    APPROPRIATE ERROR MESSAGE.
      *
      *
      *
       ENVIRONMENT DIVISION.
      *
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT SEQ-STUD-FILE
               ASSIGN TO "STUDENT.DAT".
           SELECT IND-STUD-FILE
               ASSIGN TO "STUDENT.DAI"
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS STUDENT-ID.
           SELECT SEQ-TRANS-FILE
               ASSIGN TO "STUTRANS.DAT".
           SELECT IND-TRANS-FILE
               ASSIGN TO "STUTRANS.DAI"
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS STUDENT-TRANS-ID
               ALTERNATE RECORD IS STUDENT-T-ID
                   WITH DUPLICATES.
      *
      *
      *
       DATA DIVISION.
      *
      *
       FILE SECTION.
      *
       FD  SEQ-STUD-FILE
             RECORD CONTAINS 70 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS SEQ-STUD-REC.
      *
       01  SEQ-STUD-REC               PIC X(70).
      *
       COPY FDSTUDENT OF LIBRARY.
      *
      * FD  IND-STUD-FILE
      *       RECORD CONTAINS 70 CHARACTERS
      *       LABEL RECORDS ARE OMITTED
      *       DATA RECORD IS IND-STUD-REC.
      *
      * 01  IND-STUD-REC.
      *       05  FILLER               PIC X(2).
      *       05  STUDENT-ID           PIC 9(9).
      *       05  FILLER               PIC X(59).
      *
       FD  SEQ-TRANS-FILE
             RECORD CONTAINS 46 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS SEQ-TRANS-REC.
      *
       01  SEQ-TRANS-REC              PIC X(46).
      *
       FD  IND-TRANS-FILE
             RECORD CONTAINS 46 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS IND-TRANS-REC.
      *
       01  IND-TRANS-REC.
           05  FILLER                 PIC X(2).
           05  STUDENT-TRANS-ID.
               10  STUDENT-T-ID       PIC 9(9).
               10  STUDENT-T-COUNT    PIC 9.
           05  FILLER                 PIC X(34).
      *
      *
       WORKING-STORAGE SECTION.
      *
      *
       01  PROGRAM-SWITCHES.
           05  STUD-EOF-SW            PIC X VALUE "N".
           05  TRANS-EOF-SW           PIC X VALUE "N".
      *
      *
      *
       PROCEDURE DIVISION.
      *
      *
       A00-MAIN-LINE-PARA.
           PERFORM B15-FILE-MAINT-PARA.
           PERFORM B20-INDEX-01-PARA.
           PERFORM B25-INDEX-02-PARA.
           PERFORM B15-FILE-MAINT-PARA.
           STOP RUN.
      *
      *
       B15-FILE-MAINT-PARA.
           IF STUD-EOF-SW = "N" THEN
               OPEN INPUT SEQ-STUD-FILE
                   OUTPUT IND-STUD-FILE
           ELSE
               CLOSE SEQ-STUD-FILE
                     IND-STUD-FILE
           END-IF.
 
           IF TRANS-EOF-SW = "N" THEN
               OPEN INPUT SEQ-TRANS-FILE
                   OUTPUT IND-TRANS-FILE
           ELSE
               CLOSE SEQ-TRANS-FILE
                     IND-TRANS-FILE
           END-IF.
      *
      *
       B20-INDEX-01-PARA.
           PERFORM UNTIL STUD-EOF-SW = "Y"
              READ SEQ-STUD-FILE
                  AT END
                      MOVE "Y" TO STUD-EOF-SW
                  NOT AT END
                      MOVE SEQ-STUD-REC TO IND-STUD-REC
                      WRITE IND-STUD-REC
                          INVALID KEY
                              DISPLAY "INVALID KEY"
                      END-WRITE
              END-READ
           END-PERFORM. 
      *
      *
       B25-INDEX-02-PARA.
           PERFORM UNTIL TRANS-EOF-SW = "Y"
              READ SEQ-TRANS-FILE
                  AT END
                      MOVE "Y" TO TRANS-EOF-SW
                  NOT AT END
                      MOVE SEQ-TRANS-REC TO IND-TRANS-REC
                      WRITE IND-TRANS-REC
                          INVALID KEY
                              DISPLAY "INVALID TRANS KEY"
                      END-WRITE
              END-READ
           END-PERFORM. 