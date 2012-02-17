typedef _Bool bool;
struct linux_binprm
{
  unsigned int cred_prepared:1, cap_effective:1;
};
int
cap_bprm_set_creds (struct linux_binprm *bprm)
{
  bool effective;
  bprm->cap_effective = effective;
}
