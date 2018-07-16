<apply template="default">
  <script>
  function getParameterByName(name, url) {
      if (!url) url = window.location.href;
      name = name.replace(/[\[\]]/g, '\\$&');
      var regex = new RegExp('[?&]' + name + '(=([^&#]*)|&|#|$)'),
          results = regex.exec(url);
      if (!results) return null;
      if (!results[2]) return '';
      return decodeURIComponent(results[2].replace(/\+/g, ' '));
  }

  var minLength = 6;

  function setValid() {
    var pass1 = document.getElementById("password1").value;
    var pass2 = document.getElementById("password2").value;
    var warn = document.getElementById("warning");
    if (pass1.length < minLength) {
      warn.innerHTML = "Minimum password length: " + minLength;
    }
    else if (pass1 != pass2) {
      warn.innerHTML = "Passwords need to be identical"
    }
    else {
      warn.innerHTML = "";
    }
    var valid = ((pass1.length >= minLength) && (pass1 == pass2))

    document.getElementById("submit").disabled = ! valid;
  }

  </script>
  <h1>Reset password</h1>
  <form action="./resetPasswordData">
    Password: <input oninput="setValid()" type="password" id="password1" name="password">
    Repeat password: <input oninput="setValid()" id="password2" type="password" name="password">
    <input id="submit" type="submit" value="Submit">
    <input type="hidden" name="token" id="token" value="">
    <input type="hidden" name="email" id="email" value="">
    <p id="warning"></p>
  </form>
  <script>
    var token = getParameterByName("token");
    var email = getParameterByName("email");
    document.getElementById("token").value = token;
    document.getElementById("email").value = email;
    setValid();
  </script>
</apply>
