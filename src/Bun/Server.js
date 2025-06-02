export function serve(f) {
  return function() {
    return Bun.serve({
      fetch(req) {
        return f(req)()
      }
    })
  }
}
