# exam-schedule

Generates exam schedules for a number of students.

The program is written is Haskell, and requires
[Stack](https://docs.haskellstack.org/en/stable/) to be run.

It assumes the exam to last 30 minutes, with 60 or 90 minutes of preparation,
with exam times going from 8:00 - 16:00, and a lunch break from 12:00 - 12:30.

The input is a list of names of students, prefixed with a `*` if they are to
get the longer (90 minute) preparation time.

An example input file can be seen in `example-input.txt`.

To run the program, open a shell in the project folder and run the command
`stack run`, providing the input on standard in.

An example execution can be seen below:

```
~/projects/exam-schedule$ cat example-input.txt | stack run
Score improved from 100 to 0
Day 1, 08:00 (Prep)     Room 1  Day 1, 09:00    Alma
Day 1, 08:30 (Prep)     Room 2  Day 1, 09:30    Anker
Day 1, 09:00 (Prep)     Room 1  Day 1, 10:00    Victor
Day 1, 09:30 (Prep)     Room 2  Day 1, 10:30    Ella
Day 1, 09:30 (Prep)     Room 3  Day 1, 11:00    Alfred
Day 1, 10:00 (Prep)     Room 1  Day 1, 11:30    Oscar
Day 1, 11:30 (Prep)     Room 1  Day 1, 12:30    Ida
Day 1, 11:30 (Prep)     Room 2  Day 1, 13:00    Lily
Day 1, 12:30 (Prep)     Room 1  Day 1, 13:30    Karl
Day 1, 13:00 (Prep)     Room 2  Day 1, 14:00    Marie
Day 1, 13:00 (Prep)     Room 3  Day 1, 14:30    Maja
Day 1, 14:00 (Prep)     Room 1  Day 1, 15:00    Arthur
Day 1, 14:30 (Prep)     Room 2  Day 1, 15:30    Noah
Day 2, 08:00 (Prep)     Room 1  Day 2, 09:00    Emil
Day 2, 08:30 (Prep)     Room 2  Day 2, 09:30    Oliver
Day 2, 09:00 (Prep)     Room 1  Day 2, 10:00    William
Day 2, 09:30 (Prep)     Room 2  Day 2, 10:30    Frida
Day 2, 10:00 (Prep)     Room 1  Day 2, 11:00    Olivia
Day 2, 10:00 (Prep)     Room 3  Day 2, 11:30    Lucas
Day 2, 11:00 (Prep)     Room 1  Day 2, 12:30    August
Day 2, 12:00 (Prep)     Room 2  Day 2, 13:00    Viggo
Day 2, 12:00 (Prep)     Room 3  Day 2, 13:30    Clara
Day 2, 12:30 (Prep)     Room 1  Day 2, 14:00    Alberte
Day 2, 13:30 (Prep)     Room 2  Day 2, 14:30    Otto
Day 2, 14:00 (Prep)     Room 1  Day 2, 15:00    Valdemar
Day 2, 14:00 (Prep)     Room 3  Day 2, 15:30    Johan
Day 3, 08:00 (Prep)     Room 1  Day 3, 09:00    Storm
Day 3, 08:30 (Prep)     Room 2  Day 3, 09:30    Malthe
```

The output is tab-separated, so it can be copied directly into Excel.
