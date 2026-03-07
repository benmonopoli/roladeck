let () = {
  switch (ReactDOM.querySelector("#root")) {
  | None => Js.Console.error("No #root element found")
  | Some(el) =>
    let root = ReactDOM.Client.createRoot(el);
    ReactDOM.Client.render(root, <App />);
  };
};
