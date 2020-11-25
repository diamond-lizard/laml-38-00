// The Javascript program that makes it possible to navigate
// by means of the keyboard. Special support for p, n, u, t,
// x, y, z and the extended numerical range 1..9..m.
// Work in both IE4+, Netscape6, and Mozilla.
// In this version, no special trail navigation is provided for.
// In this version, no single or double click page navigation.
// 2011: In this version navigation with 1..9 has been disabeled. Instead a - m act downward navigation.

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
// Lower and upper cases characters are equalized in this function.
// It seems to be the case that we always get upper case keycodes.
function doLenoNavigate(event){
  if (window.event){
   theEvent = window.event;}
  else{ 
   theEvent = event;
  }
  kc = theEvent.keyCode;
  //  feedback = "Keykode " + kc;
  //  window.status = feedback;
  if      (((kc == 112) || (kc == 80)) && (prevUrl != "")) {window.location = prevUrl;}      // p
  else if (((kc == 110) || (kc == 78) || (kc == 13)) && (nextUrl != "")) {window.location = nextUrl;} // n
  else if (((kc == 117) || (kc == 85)) && (upUrl != "")) {window.location = upUrl;}   // u
  else if (((kc == 84))                && (toggleUrl != "")) {window.location = toggleUrl;}   // t  ; (kc == 116) activated by F5
  else if (((kc == 120) || (kc == 88)) && (xUrl != "")) {window.location = xUrl;}   // x
  else if (((kc == 121) || (kc == 89)) && (yUrl != "")) {window.location = yUrl;}   // y
  else if (((kc == 118) || (kc == 86)) && (zUrl != "")) {window.location = zUrl;}   // z (122, 90) is not good. Using v instead.

//  Removed in sept. 2011 due to conflict with zoomit:
//  else if ((kc >= 49) && (kc <= 57)){       // 1 - 9
//        idx = kc - 49;
//        if ((idx + 1) <= downUrls.length)  
//           window.location = downUrls[idx]; 
//       }

  else if ((kc >= 97) && (kc <= 109)){       // a - m
        idx = kc - (88 + 9);
        if ((idx + 1) <= downUrls.length)  
           window.location = downUrls[idx]; 
       }
  else if ((kc >= 65) && (kc <= 77)){       // A - M
        idx = kc - (56 + 9);
        if ((idx + 1) <= downUrls.length)  
           window.location = downUrls[idx]; 
       }
}

// Navigation upon activation of the 'q', 'w', and 'r' keys resp. 
// For trail navigation FROM THE LEFTMOST TRAIL BAR ONLY
function lenoTrailbarNavigate(prevUrlPar,nextUrlPar,upUrlPar){
  prevUrl = prevUrlPar;
  nextUrl = nextUrlPar;
  upUrl = upUrlPar;
  document.onkeyup = doLenoTrailbarNavigate;
}

// Navigation upon activation of the 'q', 'w', and 'r' keys resp. 
// For trail navigation FROM THE TRAIL FRAME ONLY (THE WHOLE).
function lenoTrailframeNavigate(prevUrlPar,nextUrlPar,upUrlPar){
  prevUrl = prevUrlPar;
  nextUrl = nextUrlPar;
  upUrl = upUrlPar;
  document.onkeyup = doLenoTrailframeNavigate;
}


// Trail navigation upon activation of the 'q', 'w', 'r' keys resp.
// It seems to be the case that we always get upper case keycodes.
function doLenoTrailbarNavigate(event){
  if (window.event){
   theEvent = window.event;}
  else{ 
   theEvent = event;
  }
  kc = theEvent.keyCode;
   feedback = "Keykode " + kc;
   window.status = feedback;
  if      ((kc == 81) && (prevUrl != "")) {window.parent.location = prevUrl;}      // q
  else if ((kc == 87) && (nextUrl != "")) {window.parent.location = nextUrl;}      // w
  else if ((kc == 82) && (upUrl != "")) {window.parent.location = upUrl;}          // r
}

// Trail navigation upon activation of the 'q', 'w', 'r' keys resp.
// It seems to be the case that we always get upper case keycodes.
function doLenoTrailframeNavigate(event){
  if (window.event){
   theEvent = window.event;}
  else{ 
   theEvent = event;
  }
  kc = theEvent.keyCode;
   feedback = "Keykode " + kc;
   window.status = feedback;
  if      ((kc == 81) && (prevUrl != "")) {window.location = prevUrl;}      // q
  else if ((kc == 87) && (nextUrl != "")) {window.location = nextUrl;}      // w
  else if ((kc == 82) && (upUrl != "")) {window.location = upUrl;}          // r
}








