rsync css/style.css ec2:~
ssh ec2 'sudo mv style.css /srv/http/hask/css/style.css'

rsync css/sidebar ec2:~
ssh ec2 'sudo mv sidebar.css /srv/http/hask/css/sidebar.css'

rsync tutorial.html ec2:~
ssh ec2 'sudo mv tutorial.html /srv/http/hask/index.html'
