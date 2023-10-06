/* "pi.c", program for computing digits of numerical value of PI.
 * Copyright (C) 1991 1995 Free Software Foundation, Inc.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

/* Author: Aubrey Jaffer

pi <n> <d> prints out <n> digits of pi in groups of <d> digits.

'Spigot' algorithm origionally due to Stanly Rabinowitz.
This algorithm takes time proportional to the square of <n>/<d>.
This fact can make comparisons of computational speed between systems
of vastly differring performances quicker and more accurate.

Try: pi 100 5
The digit size <d> will have to be reduced for larger <n> or an
error due to overflow will occur. */

short *calloc();
main(c,v)
int c;char **v;{
  int n=200,j=0,m,b=2,k=0,t,r=1,d=5;
  long q;
  short *a;
  if(c>1)n=atoi(v[1]);
  if(c>2)d=atoi(v[2]);
  while(k++<d)r=r*10;
  n=n/d+1;
  k=m=3.322*n*d;
  a=calloc(1+m,2);
  while(k)a[--k]=2;
  for(a[m]=4;j<n;b=q%r){
    q=0;
    for(k=m;k;){
      q+=a[k]*r;
      t=(2*k+1);
      a[k]=q%t;
      q=q/t;
      q*=k--;}
    printf("%0*d%s",d,b+q/r,++j%10?"  ":"\n");}
  puts("");}
