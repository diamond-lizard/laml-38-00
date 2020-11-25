// Navigation upon activation of the 'p', 'n', and 'u' keys resp. Works for IE4.
// For slide, note, etc navigation
function lenoNavigate(prevUrl,nextUrl,upUrl,downUrls){
  theEvent = window.event;
  kc = theEvent.keyCode;
  if ((kc == 112) && (prevUrl != "")) window.location = prevUrl;      // p
  else if (((kc == 110) || (kc == 13)) && (nextUrl != "")) window.location = nextUrl; // n
  else if (kc == 117) window.location = upUrl;   // u
  else if ((kc >= 49) && (kc <= 57)){
        idx = kc - 49;
        if ((idx + 1) <= downUrls.length)  
           window.location = downUrls[idx]; // 1 - 9
       }
}

// Navigation upon activation of the 'P', 'N', and 'U' keys resp. Works for IE4.
// For trail navigation FROM THE LEFTMOST TRAIL BAR ONLY
function lenoTrailbarNavigate(prevUrl,nextUrl,upUrl){
  theEvent = window.event;
  kc = theEvent.keyCode;
  if ((kc == 80) && (prevUrl != "")) window.parent.location = prevUrl;      // P
  else if (((kc == 78) || (kc ==32)) && (nextUrl != "")) window.parent.location = nextUrl; // N
  else if (kc == 85) window.parent.location = upUrl;   // U
}

// Navigation upon activation of the 'P', 'N', and 'U' keys resp. Works for IE4.
// For trail navigation FROM THE TRAIL FRAME ONLY (THE WHOLE):
function lenoTrailframeNavigate(prevUrl,nextUrl,upUrl){
  theEvent = window.event;
  kc = theEvent.keyCode;
  if ((kc == 80) && (prevUrl != "")) window.location = prevUrl;      // P
  else if (((kc == 78) || (kc ==32)) && (nextUrl != "")) window.location = nextUrl; // N
  else if (kc == 85) window.location = upUrl;   // U
}



