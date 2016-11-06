rsync css/style.css ec2:~
ssh ec2 'sudo mv style.css /srv/http/hask/css/style.css'

rsync css/layout.css ec2:~
ssh ec2 'sudo mv sidebar.css /srv/http/hask/css/layout.css'

rsync js/nav.js ec2:~
ssh ec2 'sudo mv nav.js /srv/http/hask/nav.js'

rsync tutorial.html ec2:~
ssh ec2 'sudo mv tutorial.html /srv/http/hask/index.html'
