export function json(j) {
  return function(options) {
    return Response.json(j, options)
  }
}


export function string(s) {
  return function(options) {
    return new Response(s, options)
  }
}
