{\rtf1\ansi\ansicpg1252\cocoartf1561\cocoasubrtf600
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww12600\viewh7800\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 (define (enumerate tree)\
    (cond ((null? tree) '())\
          ((pair? tree) (append (enumerate (car tree)) (enumerate (cdr tree))))\
          (else (list tree))\
          )\
    )\
\
(define (enumerate-2 tree)\
    (define (recur tree accum)\
        (cond ((null? tree) accum)\
              ((pair? tree) (recur (car tree) (recur (cdr tree) accum)))\
              (else (cons tree accum))\
              )\
        )\
    (recur tree '())\
    )\
}