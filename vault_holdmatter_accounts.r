# Google Apps Manager (ADV-XTD3 version)
# https://github.com/taers232c/GAMADV-XTD3/wiki/Vault#display-vault-holds
# gam print vaultholds shownames > legal_hold_check.csv

# Import list of open Google Vault holdmatters and generate list of all unique accounts
# across all holdmatters

vault_holdmatter_accounts <- read_csv("legal_hold_check.csv") %>% 
  select(matterId,matterName, holdId,name,contains("email")) %>% 
  pivot_longer(
    cols = starts_with("accounts"),
    names_to = "email_account",
    values_to = "email_address",
    values_drop_na = TRUE
  ) %>% 
  select(email_address) %>% 
  unique(.) %>% 
  arrange(email_address) %>% 
  mutate_all(tolower)