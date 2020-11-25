// Navigation upon activation of the 'p', 'n', 'u', and 'r' keys resp.
// Lower and upper cases characters are equalized in this function.
// It seems to be the case that we always get upper case keycodes.
// Work in both IE4+, Netscape6, and Mozilla.
function photoNavigate(nextUrlPar,prevUrlPar,runUrlPar,upUrlPar){
  nextUrl = nextUrlPar;
  prevUrl = prevUrlPar;
  runUrl = runUrlPar;
  upUrl = upUrlPar;
  document.onkeyup = doPhotoNavigate;
}

function doPhotoNavigate(event){
  if (window.event){
   theEvent = window.event;}
  else{ 
   theEvent = event;
  }
  kc = theEvent.keyCode;
  if (((kc == 110) || (kc == 78) || (kc == 13)) && (nextUrl != "")) window.location = nextUrl;          // n and CR
  else if (((kc == 112) || (kc == 80)) && (prevUrl != "")) window.location = prevUrl;                   // p
  else if (((kc == 114) || (kc == 82) || (kc == 32)) && (prevUrl != "")) window.location = runUrl;      // r and space
  else if (((kc == 117) || (kc == 85)) && (upUrl != "")) window.location = upUrl;                       // u
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
