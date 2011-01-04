c   mini-explor.f
c   A re-implementation of Ken Knowlton's MINI-EXPLOR for GNU Fortran
c   Copyright (C) 2006 Rob Myers rob@robmyers.org
c   This file is part of explor.
c
c    explor is free software: you can redistribute it and/or modify
c    it under the terms of the GNU General Public License as published by
c    the Free Software Foundation, either version 3 of the License, or
c    (at your option) any later version.
c
c    explor is distributed in the hope that it will be useful,
c    but WITHOUT ANY WARRANTY; without even the implied warranty of
c    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c    GNU General Public License for more details.
c
c    You should have received a copy of the GNU General Public License
c    along with explor.  If not, see <http://www.gnu.org/licenses/>.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Picture Internals
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Initialise the array

      subroutine initpic()
      integer pic(140, 140)
      common /pic/ pic
      do 20 j = 1, 140
         do 10 i = 1, 140
            pic(i, j) = 0
 10      continue
 20   continue
      return
      end

c Copy the array

      subroutine copypic(to)
      integer to(140,140)
      integer pic(140, 140)
      common /pic/ pic
      do 20 j = 1, 140
         do 10 i = 1, 140
            to(i, j) = pic(i,j)
 10      continue
 20   continue
      return
      end

c Write picture cell values as letters
c Rationalise and optimise
c xo, yo are the origins, in cell offsets
c 7 and 8 have been chosen to give teletype-looking offsets for characters

      subroutine writelet(x, y)
      integer x, y
      integer val, xx, yy 
      if(x .GE. 1 .AND. y .GE. 1 .AND. x .LE. 140 .AND. y .LE. 140) then
         val = num(x, y)
         xx = (x - 1) * 7
         yy = (y - 1) * 8
         if (val .EQ. 0) then
            write(*,*) xx, " ", yy, " moveto"
            write(*,*) "( ) show"
         elseif (val .EQ. 1) then
            write(*,*) xx, " ", yy, " moveto"
            write(*,*) "(\\') show"
         elseif  (val .EQ. 2) then
            write(*,*) xx, " ", yy, " moveto"
            write(*,*) "(x) show"
            write(*,*) xx, " ", yy, " moveto"
            write(*,*) "(-) show"
         elseif  (val .EQ. 3) then
            write(*,*) xx, " ", yy, " moveto"
            write(*,*) "(0) show"
            write(*,*) xx, " ", yy, " moveto"
            write(*,*) "(H) show"
            write(*,*) xx, " ", yy, " moveto"
            write(*,*) "(X) show"
         endif
      endif
      return
      end

c Write the picture as a file
c FIXME: we need to lose the space at the start of each line!
c        otherwise Preview.app doesn't recognise the EPS bounds!

      subroutine writelets(x, y, w, h)
      integer x, y, w, h
      integer left, right, bottom, top
      integer i, j, a, b, c, d, aa, bb, cc, now
      call xywh( x, y, w, h, left, right, bottom, top )
      write(*,*) "%!PS-Adobe-3.0 EPSF-3.0"
      write(*,*) "%%BoundingBox: 0 0", 
     +     7 * min(w, 140), 8 * min(h, 140)
      write(*,*) "/Courier findfont"
      write(*,*) "12 scalefont setfont"
      do 20 j = bottom, top
         do 10 i = left, right
            call writelet(i, j)
   10    continue
   20 continue
      write(*,*) "%%EOF"
      return
      end

c Converts an x,y,w,h box to an edge-position box
 
      subroutine xywh(x, y, w, h, left, right, bottom, top)
      integer x, y, w, h
      integer left, right, bottom, top
      left = x - (w / 2)
      right = x + (w / 2)
      if(mod(w, 2) .EQ. 1) then
         left = left - 1
      endif
      bottom = y - (h / 2)
      top = y + (h / 2)
      if(mod(h, 2) .EQ. 1) then
         bottom = bottom - 1
      endif
      return 
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c The Public API
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c initexplor
c initialization routine that should be called automatically

      subroutine initexplor
c     The random number generator
      integer seed
      common /random/ seed

c     Initialise the random number generator
      seed = 13

c     Initialise the picture cell array
      call initpic

      return
      end

c num

      integer function num (x,y)
      integer x, y
      integer pic(140, 140)
      common /pic/ pic
      if( x .GE. 1 .AND. x .LE. 140 .AND. y .GE. 1 .AND. y .LE. 140) 
     +     then
         num = pic(x, y)
      else
         num = 4
      endif
      return
      end

c ne
c generate random number between min and max
c min may be > max, either may be negative
c but the difference between them must be < 199
c Adapted from Clocksin & Mellish :-)

      integer function ne (mmin, mmax)
      integer mmin, mmax
      integer seed
      common /random/ seed
      integer tmp, range
      if(mmin .GT. mmax) then
         tmp = mmin
         mmin = mmax
         mmax = tmp
      endif 
      range = abs(mmax - mmin)
      if(range .GE. 199) then
         range = 198
      endif
      ne = mmin + mod(seed, range) + 1
      seed = mod( (125 * seed + 1), 4096)
      return
      end

c put

      subroutine put (x, y, n)
      integer x, y, n
      integer pic(140, 140)
      common /pic/ pic
      if (n .GE. 0 .AND. n .LE. 3) then
         if(x . GE. 1 .AND. x .LE. 140 .AND. 
     +        y .GE. 1 .AND. y .LE. 140) then
            pic(x,y) = n
         endif
      endif
      return 
      end

c put4
c Use division and remainder to extract each digit of the number

      subroutine n4n (n, n1, n2, n3, n4)
      integer n, n1, n2, n3, n4
      integer r1, r2
      n1 = n / 1000
      n2 = mod(n, 1000) / 100
      n3 = mod(n, 100) / 10
      n4 = mod(n, 10)
      return 
      end

      subroutine put4 (x, y, n)
      integer x, y, n
      integer n1, n2, n3, n4
      call n4n(n, n1, n2, n3, n4)
      call put(x, y, n1)
      call put(x + 1, y, n2)
      call put(x + 2, y, n3)
      call put(x + 3, y, n4)
      return
      end

c put16

      subroutine put16 (x, y, n1, n2, n3, n4)
      integer x, y, n1, n2, n3, n4
      call put4(x, y, n1)
      call put4(x + 4, y, n2)
      call put4(x + 8, y, n3)
      call put4(x + 12, y, n4)
      return
      end

c chanj

      subroutine chanj (x, y, w, h, percent, rule)
      integer x, y, w, h, percent, rule
      integer left, right, bottom, top
      integer i, j, a, b, c, d, aa, bb, cc, now
      call xywh( x, y, w, h, left, right, bottom, top )
      a = rule / 1000
      aa = mod(rule, 1000)
      b = aa / 100
      bb = mod(aa, 100)
      c = bb / 10
      d = mod(bb, 10)
      do 20 j = bottom, top
         do 10 i = left, right 
            if(percent .EQ. 100 .OR. ne(0, 100) .LT. percent) then
               now = num(i, j)
               if(now .EQ. 0) then
                  call put(i, j, a)
               elseif(now .EQ. 1) then
                  call put(i, j, b)
               elseif(now .EQ. 0) then
                  call put(i, j, c)
               elseif(now .EQ. 0) then
                  call put(i, j, d)
               endif
            endif
   10    continue
   20 continue
      return
      end

c locop
c Copy the array to avoid propagating changes

      subroutine locop (x, y, w, h, prob, ok, nebor, these, rule)
      integer x, y, w, h, prob, ok, nebor, these, rule
      integer left, right, bottom, top
      integer oldpic(140, 140)
      integer col1, col2, col3
      integer tl, l, bl, t, b, tr, cr, br
      integer i, j
      call xywh( x, y, w, h, left, right, bottom, top )
c     Get a copy of the picture so we take can values from it unmodified
      call copypic(oldpic)
c     Get each column number
      col1 = mod(rule, 1000) / 100
      col2 = mod(rule, 100) / 10
      col3 = mod(rule, 10)
c     Get each component of each column (if it's present it's > 0)
      tl = (col1 / 100)
      t = (col2 / 100)
      tr =  (col3 / 100)
      l = (mod(col1, 100) / 10)
      r = (mod(col3, 100) / 10)
      bl = (mod(col1, 10) / 10)
      b = (mod(col2, 10) / 10)
      br = (mod(col3, 10) / 10)
c     Loop through, processing each tile
      do 20 j = bottom, top
         do 10 i = left, right 
c     Constrain bottom/top/left/right so we don't have to do this
            if(i .GE. 1 .AND. i .LE. 140 .AND. j .GE. 1 .AND. 
     +           j .LE. 140) then
               if(percent .EQ. 100 .OR. ne(0, 100) .LT. percent) then
c     Do It...
                  write(*,*) 1
               endif
            endif
 10      continue
 20   continue
      return
      end
      
c show

      subroutine show (x, y, w, h)
      integer x, y, w, h
      call writelets(x, y, w, h)
      return
      end

c combn
c Cells are processed left-to-right, bottom-to-top

c Utility function to combine cell values

      integer function numcombn (to, from, r0, r1, r2, r3)
      integer from, to, r0, r1, r2, r3
      integer rule, rr0, rr1, rr2, rr3
      integer r
      if(from .EQ. 0) rule = r0
      if(from .EQ. 1) rule = r1
      if(from .EQ. 2) rule = r2
      if(from .EQ. 3) rule = r3
      call n4n(rule, rr0, rr1, rr2, rr3)
      if(to .EQ. 0) numcombn = rr0
      if(to .EQ. 1) numcombn = rr1
      if(to .EQ. 2) numcombn = rr2
      if(to .EQ. 3) numcombn = rr3
      return
      end

c Straight copy

      subroutine combn1 (x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
      integer x, y, w, h, prob, xf, yf, r0, r1, r2, r3
      integer left1, right1, bottom1, top1
      integer left2, right2, bottom2, top2
      integer i, j, n
      call xywh(x, y, w, h, left1, right1, bottom1, top1)
      call xywh(xf, yf, w, h, left2, right2, bottom2, top2)
      do 1 i = 1, w
         do 2 j = 1, h
            if((prob .EQ. 100) .OR. (ne(0,99) .LT. prob)) then
               n = numcombn(num(left1 + i, bottom1 + j),
     +              num(left2 + i, bottom2 + j), r0, r1, r2, r3)
               call put(left1 + i, bottom1 + j, n)
            endif
 2       continue
 1    continue
      return
      end

c Rotate 90 degrees clockwise

      subroutine combn2 (x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
      integer x, y, w, h, prob, xf, yf, r0, r1, r2, r3
      integer left1, right1, bottom1, top1
      integer left2, right2, bottom2, top2
      integer i, j, n
      call xywh(x, y, w, h, left1, right1, bottom1, top1)
      call xywh(xf, yf, h, w, left2, right2, bottom2, top2)
      do 1 i = 1, w
         do 2 j = 1, h
            if(prob .EQ. 100 .OR. ne(0,99) .LT. prob) then
               n = numcombn(num(left1 + i, bottom1 + j),
     +              num((right2 - j) + 1, bottom2 + i), r0, r1, r2, r3)
               call put(left1 + i, bottom1 + j, n)
            endif
 2       continue
 1    continue
      return
      end

c Rotate 180 degrees

      subroutine combn3 (x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
      integer x, y, w, h, prob, xf, yf, r0, r1, r2, r3
      integer left1, right1, bottom1, top1
      integer left2, right2, bottom2, top2
      integer i, j, n
      call xywh(x, y, w, h, left1, right1, bottom1, top1)
      call xywh(xf, yf, w, h, left2, right2, bottom2, top2)
      do 1 i = 1, w
         do 2 j = 1, h
            if(prob .EQ. 100 .OR. ne(0,99) .LT. prob) then
               n = numcombn(num(left1 + i, bottom1 + j),
     +              num((right2 - i) + 1, (top2 - j) + 1), 
     +              r0, r1, r2, r3)
               call put(left1 + i, bottom1 + j, n)
            endif
 2       continue
 1    continue
      return
      end

c  Rotate 90 degrees anticlockwise

      subroutine combn4 (x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
      integer x, y, w, h, prob, xf, yf, r0, r1, r2, r3
      integer left1, right1, bottom1, top1
      integer left2, right2, bottom2, top2
      integer i, j, n
      call xywh(x, y, w, h, left1, right1, bottom1, top1)
      call xywh(xf, yf, h, w, left2, right2, bottom2, top2)
      do 1 i = 1, w
         do 2 j = 1, h
            if(prob .EQ. 100 .OR. ne(0,99) .LT. prob) then
               n = numcombn(num(left1 + i, bottom1 + j),
     +              num(left2 + j, (top2 - i) + 1), r0, r1, r2, r3)
               call put(left1 + i, bottom1 + j, n)
            endif
 2       continue
 1    continue
      return
      end

c Flip right/left

      subroutine combn5 (x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
      integer x, y, w, h, prob, xf, yf, r0, r1, r2, r3
      integer left1, right1, bottom1, top1
      integer left2, right2, bottom2, top2
      integer i, j, n
      call xywh(x, y, w, h, left1, right1, bottom1, top1)
      call xywh(xf, yf, w, h, left2, right2, bottom2, top2)
      do 1 i = 1, w
         do 2 j = 1, h
            if(prob .EQ. 100 .OR. ne(0,99) .LT. prob) then
               n = numcombn(num(left1 + i, bottom1 + j),
     +              num((right2 - i) + 1, bottom2 + j), r0, r1, r2, r3)
               call put(left1 + i, bottom1 + j, n)
            endif
 2       continue
 1    continue
      return
      end

c Right/left flip and rotate 90 degrees clockwise

      subroutine combn6 (x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
      integer x, y, w, h, prob, xf, yf, r0, r1, r2, r3
      integer left1, right1, bottom1, top1
      integer left2, right2, bottom2, top2
      integer i, j, n
      call xywh(x, y, w, h, left1, right1, bottom1, top1)
      call xywh(xf, yf, h, w, left2, right2, bottom2, top2)
      do 1 i = 1, w
         do 2 j = 1, h
            if(prob .EQ. 100 .OR. ne(0,99) .LT. prob) then
               n = numcombn(num(left1 + i, bottom1 + j),
     +              num(left2 + j, bottom2 + i), r0, r1, r2, r3)
               call put(left1 + i, bottom1 + j, n)
            endif
 2       continue
 1    continue
      return
      end

c Right/left flip and rotate 180 degrees clockwise
      
      subroutine combn7 (x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
      integer x, y, w, h, prob,xf, yf, r0, r1, r2, r3
      integer left1, right1, bottom1, top1
      integer left2, right2, bottom2, top2
      integer i, j, n
      call xywh(x, y, w, h, left1, right1, bottom1, top1)
      call xywh(xf, yf, w, h, left2, right2, bottom2, top2)
      do 1 i = 1, w
         do 2 j = 1, h
            if(prob .EQ. 100 .OR. ne(0,99) .LT. prob) then
               n = numcombn(num(left1 + i, bottom1 + j),
     +              num(left2 + i, (top2 - j) + 1), r0, r1, r2, r3)
               call put(left1 + i, bottom1 + j, n)
            endif
 2       continue
 1    continue
      return
      end

c Right/left flip and rotate 90 degrees anticlockwise

      subroutine combn8 (x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
      integer x, y, w, h, prob, xf, yf, r0, r1, r2, r3
      integer left1, right1, bottom1, top1
      integer left2, right2, bottom2, top2
      integer i, j, n
      call xywh(x, y, w, h, left1, right1, bottom1, top1)
      call xywh(xf, yf, h, w, left2, right2, bottom2, top2)
      do 1 i = 1, w
         do 2 j = 1, h
            if(prob .EQ. 100 .OR. ne(0,99) .LT. prob) then
               n = numcombn(num(left1 + i, bottom1 + j),
     +              num((right2 - j) + 1, (top2 - i) + 1), 
     +              r0, r1, r2, r3)
               call put(left1 + i, bottom1 + j, n)
            endif
 2       continue
 1    continue
      return
      end

c Main combn function

      subroutine combn (x, y, w, h, prob, xf, yf, orient, 
     +     r0, r1, r2, r3)
      integer x, y, w, h, prob, xf, yf, orient, r0, r1, r2, r3
      if(ne(0,100) .LT. prob) then
         if(orient .EQ. 1) then 
            call combn1(x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
         endif
         if(orient .EQ. 2) then
            call combn2(x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
         endif
         if(orient .EQ. 3) then
            call combn3(x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
         endif
         if(orient .EQ. 4) then
            call combn4(x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
         endif
         if(orient .EQ. 5) then 
            call combn5(x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
         endif
         if(orient .EQ. 6) then  
            call combn6(x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
         endif
         if(orient .EQ. 7) then  
            call combn7(x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
         endif
         if(orient .EQ. 8) then 
            call combn8(x, y, w, h, prob, xf, yf, r0, r1, r2, r3)
         endif
      endif
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Initialise the library
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      call initexplor
      end
