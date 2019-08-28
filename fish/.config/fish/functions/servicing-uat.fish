function servicing-uat
  kubectl exec -n uat -it (kubectl get pods -n uat -l product=servicing,app=servicing-rails-webserver -o=custom-columns=NAME:.metadata.name | tail -1) bash
end
