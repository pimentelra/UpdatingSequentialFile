      $ SET SOURCEFORMAT "FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. SeqUpdate.
AUTHOR. Michael Coughlan.
* This program takes records from an ordered Transaction File (Tranfer.Dat)
* and uses them to update records of the ordered Student File (Students.Dat)
* by creating a new file (Students.New) containing the updated records.
* It then displays the updated records.
* The program assumes that there will not be more than one transaction
* per student record.
* The program detects two types of error condition;
*     1. It detects when there is no corresponding record in the student file
*        for a record in the transaction file.
*     2. In matching records it detects when the transaction OldCourseCode                                                  ion record
*        is not the same as the CourseCode in the student record.


ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
      SELECT StudentFile ASSIGN "STUDENTS.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

      SELECT TransFile ASSIGN "TRANSFER.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

      SELECT NewStudentFile ASSIGN "STUDENTS.NEW"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.


DATA DIVISION.
FILE SECTION.
FD StudentFile.
01 StudentRecord.
   88 EndOfStudentFile     VALUE ALL "9".
   02 StudentID-S          PIC 9(7).
   02 FILLER               PIC X(16).
   02 CourseCode-S	   PIC X(4).
   02 FILLER               PIC X(5).

FD TransFile.
01 TransRecord.
   88 EndOfTransFile       VALUE ALL "9".
   02 StudentID-T          PIC 9(7).
   02 OldCourseCode-T      PIC X(4).
   02 NewCourseCode-T      PIC X(4).

FD NewStudentFile.
01 NewStudentRecord.
   88 EndOfNewStudentFile  VALUE HIGH-VALUES.
   02 StudentID-N          PIC 9(7).
   02 StudentName-N        PIC X(10).
   02 FILLER               PIC X(6).
   02 CourseCode-N         PIC X(4).
   02 FILLER               PIC X(5).



PROCEDURE DIVISION.
BEGIN.
* First apply the transactions to the students file
    OPEN INPUT StudentFile
    OPEN INPUT TransFile
    OPEN OUTPUT NewStudentFile

    READ StudentFile
       AT END SET EndOfStudentFile TO TRUE
    END-READ

    READ TransFile
       AT END SET EndOfTransFile TO TRUE
    END-READ

    PERFORM ApplyTrans UNTIL (EndOfStudentFile) AND (EndOfTransFile)
    
    CLOSE StudentFile
    CLOSE TransFile
    CLOSE NewStudentFile

* Then display the contents of the merged file
    PERFORM DisplayNewFile
    
    STOP RUN.

ApplyTrans.
    EVALUATE          TRUE
      WHEN (StudentID-T < StudentId-S) 
           DISPLAY "Error - Student " StudentId-T " does not exist"
           READ TransFile
              AT END SET EndOfTransFile TO TRUE
           END-READ
      WHEN (StudentID-T = StudentId-S)
           IF  OldCourseCode-T = CourseCode-S THEN
                 MOVE NewCourseCode-T TO CourseCode-S
             ELSE
                 DISPLAY "Error in " StudentId-T " CourseCode mismatch"
           END-IF
           WRITE NewStudentRecord FROM StudentRecord
           READ TransFile
              AT END SET EndOfTransFile TO TRUE
           END-READ
           READ StudentFile
             AT END SET EndOfStudentFile TO TRUE
           END-READ
      WHEN (StudentId-T > StudentId-S)
           WRITE NewStudentRecord FROM StudentRecord
           READ StudentFile
             AT END SET EndOfStudentFile TO TRUE
           END-READ
    END-EVALUATE.


DisplayNewFile.
    OPEN INPUT NewStudentFile
    DISPLAY "Stud-Id Stud-Name  Course"
    READ NewStudentFile
      AT END SET EndOfNewStudentFile TO TRUE
    END-READ 
    PERFORM UNTIL EndOfNewStudentFile
       DISPLAY StudentId-N SPACE StudentName-N SPACE CourseCode-N
       READ NewStudentFile
           AT END SET EndOfNewStudentFile TO TRUE
       END-READ
    END-PERFORM
    CLOSE NewStudentFile.


