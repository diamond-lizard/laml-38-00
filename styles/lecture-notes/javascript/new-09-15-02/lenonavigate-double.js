var prevUrl, nextUrl, upUrl, toggleUrl, xUrl, yUrl, zUrl, downUrls;

function lenoNavigate(prevUrlPar,nextUrlPar,upUrlPar,toggleUrlPar,xUrlPar,yUrlPar,zUrlPar,downUrlsPar){
  prevUrl = prevUrlPar;
  nextUrl = nextUrlPar;
  upUrl = upUrlPar;
  toggleUrl = toggleUrlPar;
  xUrl = xUrlPar;
  yUrl = yUrlPar;
  zUrl = zUrlPar;
  downUrls = downUrlsPar;
  document.onkeyup = doLenoNavigate;
}

// Navigation upon activation of the 'p', 'n', 'u', and 't' keys resp.
// And also 'x', 'y', and 'z' for various extension purposes
// Also '1', ..., '9' together with 'a' .. 'm' (downward navigagion).
function doLenoNavigate(event){
  if (window.event){
   theEvent = window.event;}
  else{ 
   theEvent = event;
  }
  kc = theEvent.keyCode;
  feedback = "Keykode " + kc;
  window.status = feedback;
  if ((kc == 112) && (prevUrl != "")) {window.location = prevUrl;}      // p
  else if (((kc == 110) || (kc == 13)) && (nextUrl != "")) {window.location = nextUrl;} // n
  else if ((kc == 117) && (upUrl != "")) {window.location = upUrl;}   // u
  else if ((kc == 116) && (toggleUrl != "")) {window.location = toggleUrl;}   // t
  else if ((kc == 120) && (xUrl != "")) {window.location = xUrl;}   // x
  else if ((kc == 121) && (yUrl != "")) {window.location = yUrl;}   // y
  else if ((kc == 122) && (zUrl != "")) {window.location = zUrl;}   // z
  else if ((kc >= 49) && (kc <= 57)){       // 1 - 9
        idx = kc - 49;
        if ((idx + 1) <= downUrls.length)  
           window.location = downUrls[idx]; 
       }
  else if ((kc >= 97) && (kc <= 109)){       // a - m
        idx = kc - 88;
        if ((idx + 1) <= downUrls.length)  
           window.location = downUrls[idx]; 
       }
}








