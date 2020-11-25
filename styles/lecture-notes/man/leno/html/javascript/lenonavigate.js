// Navigation upon activation of the 'p', 'n', and 'u' keys resp. Works for IE4.
function lenoNavigate(prevUrl,nextUrl,upUrl){
  theEvent = window.event;
  kc = theEvent.keyCode;
  if ((kc == 112) && (prevUrl.length > 0)) window.location = prevUrl;      // p
  else if ((kc == 110) && (nextUrl.length > 0)) window.location = nextUrl; // n
  else if ((kc == 117) && (upUrl.length > 0)) window.location = upUrl;     // u
}
