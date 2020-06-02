#' Finding out the kind of triangle which you are having.
#'
#' This function will tell you the kind of triangle which you are having by using 'Pythagorean theorem'.
#' But I found out that the code 'sqrt(2)^2-2' return 4.440892e-16 instead of 0.
#' That is the reason I made the code 'xa <- round(a^2) ~ xc <- round(c^2)' in this function.
#' And for example, if lengths of triangles's each side are 1.6, 1.6, sqrt(5.12), This triangle must be right-angled triangle.
#' But this function will return "This is isosceles,acute-angled triangle."
#' Thus, I will limit the kind of values of triangles's each side.
#' Only when these values are integer or the form 'sqrt(n)'(n must be natural number.), This function will be executed correctly.
#'
#' @examples
#'
#' WhatKindOfTriangle(2,2,sqrt(8))
WhatKindOfTriangle <- function(a,b,c){
  if(a <= 0 | b <= 0 | c <= 0 |a >= b + c |b >= a + c|c >= a + b){
    print("There is an error!, This is not triangle. Please check lengths of triangles's each side.")
  } else if(a == b & b == c){
    print("This is equilateral triangle.")
  } else if(a == b){
    xa<-round(a^2)
    xb<-round(b^2)
    xc<-round(c^2)
    if(xa + xb == xc){
      print("This is isosceles, right-angled triangle.")
    } else if(xa + xb > xc){
      print("This is isosceles, acute-angled triangle.")
    } else{
      print("This is isosceles, obtuse-angled triangle.")
    }
  } else if(b == c){
    xa<-round(a^2)
    xb<-round(b^2)
    xc<-round(c^2)
    if(xb + xc == xa){
      print("This is isosceles, right-angled triangle.")
    } else if(xb + xc > xa){
      print("This is isosceles, acute-angled triangle.")
    } else{
      print("This is isosceles, obtuse-angled triangle.")
    }
  } else if(a == c){
    xa<-round(a^2)
    xb<-round(b^2)
    xc<-round(c^2)
    if(xa + xc == xb){
      print("This is isosceles, right-angled triangle.")
    } else if(xa + xc > xb){
      print("This is isosceles, acute-angled triangle.")
    } else{
      print("This is isosceles, obtuse-angled triangle.")
    }
  } else if (a > c & a > b){
    xa<-round(a^2)
    xb<-round(b^2)
    xc<-round(c^2)
    if(xa == xb + xc){
      print("This is right-angled triangle, but not isosceles.")
    } else if(xa > xb + xc){
      print("This is obtuse-angled triangle, but not isosceles.")
    } else{
      print("This is acute-angled triangle, but not isosceles.")
    }
  } else if (b > a & b > c){
    xa<-round(a^2)
    xb<-round(b^2)
    xc<-round(c^2)
    if(xb == xa + xc){
      print("This is right-angled triangle, but not isosceles.")
    } else if(xb > xa + xc){
      print("This is obtuse-angled triangle, but not isosceles.")
    } else{
      print("This is acute-angled triangle, but not isosceles.")
    }
  } else if (c > a & c > b){
    xa<-round(a^2)
    xb<-round(b^2)
    xc<-round(c^2)
    if(xc == xa + xb){
      print("This is right-angled triangle, but not isosceles.")
    } else if(xc > xa + xb){
      print("This is obtuse-angled triangle, but not isosceles.")
    } else{
      print("This is acute-angled triangle, but not isosceles.")
    }
  }

}
