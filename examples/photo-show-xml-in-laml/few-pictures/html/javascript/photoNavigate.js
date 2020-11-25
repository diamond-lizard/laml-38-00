// Navigation upon activation of the 'p', 'n', 'u', and 'r' keys resp.
// Lower and upper cases characters are equalized in this function.
// It seems to be the case that we always get upper case keycodes.
// Work in both IE4+, Netscape6, and Mozilla.
function photoNavigate(nextUrlPar,prevUrlPar,runUrlPar,upUrlPar,xUrlPar,yUrlPar,zUrlPar,downUrlsPar){
  nextUrl = nextUrlPar;
  prevUrl = prevUrlPar;
  runUrl = runUrlPar;
  upUrl = upUrlPar;
  xUrl = xUrlPar;
  yUrl = yUrlPar;
  zUrl = zUrlPar;
  downUrls = downUrlsPar;
  document.onkeyup = doPhotoNavigate;
}

function ClipBoard(){
  holdtext.innerText = copytext.innerText;
  Copied = holdtext.createTextRange();
  Copied.execCommand("Copy");
}

function doPhotoNavigate(event){
  if (window.event){
   theEvent = window.event;}
  else{ 
   theEvent = event;
  }
  kc = theEvent.keyCode;

  // alert(kc);

  if ((kc == 81) || (kc == 113))    // q or Q
     ClipBoard();

  if (((kc == 110) || (kc == 78) || (kc == 13)) && (nextUrl != "")) window.location = nextUrl;          // n and CR
  else if (((kc == 112) || (kc == 80)) && (prevUrl != "")) window.location = prevUrl;                   // p
  else if (((kc == 114) || (kc == 82) || (kc == 32)) && (runUrl != "")) window.location = runUrl;      // r and space
  else if (((kc == 117) || (kc == 85)) && (upUrl != "")) window.location = upUrl;                       // u
  else if (((kc == 120) || (kc == 88)) && (xUrl != "")) {window.location = xUrl;}   // x
  else if (((kc == 121) || (kc == 89)) && (yUrl != "")) {window.location = yUrl;}   // y
  else if (((kc == 122) || (kc == 90)) && (zUrl != "")) {window.location = zUrl;}   // z
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
  else if ((kc >= 65) && (kc <= 77)){       // A - M
        idx = kc - 56;
        if ((idx + 1) <= downUrls.length)  
           window.location = downUrls[idx]; 
       }
}


// Similar functions used for keyboard navigation of timed pages:
function photoNavigateRunning(stopUrlPar){
  stopUrl = stopUrlPar;
  document.onkeyup = doPhotoNavigateRunning;
}

function doPhotoNavigateRunning(event){
  if (window.event){
   theEvent = window.event;}
  else{ 
   theEvent = event;
  }
  kc = theEvent.keyCode;
  if (((kc == 115) || (kc == 83) || (kc == 13) || (kc == 32)) && (stopUrl != "")) window.location = stopUrl; // s
}

