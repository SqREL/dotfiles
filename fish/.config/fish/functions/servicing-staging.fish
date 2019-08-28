function servicing-staging
  kubectl exec -n staging -it (kubectl get pods -n staging -l product=servicing,app=servicing-rails-webserver -o=custom-columns=NAME:.metadata.name | tail -1) bash
end
