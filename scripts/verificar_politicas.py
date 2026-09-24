#!/usr/bin/env python3
"""Auditoria somente de leitura dos guias e rulesets do datajud."""

import argparse
import json
from pathlib import Path
import re
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[1]
REPO = "rfdornelles/datajud"
CHECKS = {"R 4.2", "Medir cobertura no R 4.2"}


def exigir(condicao, mensagem):
    if not condicao:
        raise ValueError(mensagem)


def ler_configuracoes():
    configs = {}
    for nome in ("protect", "main-ci-obrigatoria"):
        config = json.loads((ROOT / ".github/rulesets" / f"{nome}.json").read_text())
        exigir(config["name"] == nome, f"Nome inesperado: {nome}")
        exigir(config["enforcement"] == "active", f"Ruleset inativo: {nome}")
        exigir(config["target"] == "branch", f"Alvo inesperado: {nome}")
        exigir(config["conditions"] == {"ref_name": {"include": ["~DEFAULT_BRANCH"], "exclude": []}},
               f"Seleção de branch inesperada: {nome}")
        configs[nome] = config
    exigir(configs["main-ci-obrigatoria"]["bypass_actors"] == [], "CI não pode ter bypass")
    exigir(configs["protect"]["bypass_actors"] == [
        {"actor_id": 31113760, "actor_type": "User", "bypass_mode": "pull_request"}
    ], "Bypass deve ser exclusivo do mantenedor e limitado a PRs")
    regras = {r["type"]: r for r in configs["protect"]["rules"]}
    exigir({"deletion", "non_fast_forward", "required_signatures", "pull_request"} <= regras.keys(),
           "Proteções existentes ausentes")
    pr = regras["pull_request"]["parameters"]
    exigir(pr["required_approving_review_count"] == 0 and pr["require_code_owner_review"]
           and not pr["require_last_push_approval"]
           and not pr["require_extra_approval_for_unattributed_changes"]
           and not pr["required_reviewers"] and pr["required_review_thread_resolution"],
           "CODEOWNERS deve ser obrigatório, sem aprovação genérica adicional; a exceção do mantenedor é por PR")
    for nome, config in configs.items():
        checks = next(r for r in config["rules"] if r["type"] == "required_status_checks")["parameters"]
        exigir(checks["strict_required_status_checks_policy"], f"CI deve exigir branch atualizada: {nome}")
        exigir({c["context"] for c in checks["required_status_checks"]} == CHECKS,
               f"Checks diferentes dos workflows: {nome}")
        exigir(all(c["integration_id"] == 15368 for c in checks["required_status_checks"]),
               "Checks devem vir do GitHub Actions")
    workflows = "\n".join(p.read_text() for p in (ROOT / ".github/workflows").glob("*.yml"))
    for check in CHECKS:
        exigir(re.search(r"^\s+name:\s*" + re.escape(check) + r"\s*$", workflows, re.M),
               f"Workflow não publica o check {check}")
    return configs


def verificar_links():
    nomes = ["CONTRIBUTING.md", "COMPATIBILIDADE.md", "RELEASE.md", ".github/rulesets/README.md"]
    for nome in nomes:
        arquivo = ROOT / nome
        for alvo in re.findall(r"\]\(([^)]+)\)", arquivo.read_text()):
            if re.match(r"https?://", alvo):
                continue
            caminho = alvo.split("#", 1)[0]
            exigir((arquivo.parent / caminho).exists(), f"Link local inexistente em {nome}: {alvo}")


def api(caminho):
    result = subprocess.run(["gh", "api", caminho], check=True, capture_output=True, text=True)
    return json.loads(result.stdout)


def conferir_subconjunto(esperado, atual, caminho):
    if isinstance(esperado, dict):
        exigir(isinstance(atual, dict), f"Objeto remoto inválido: {caminho}")
        for chave, valor in esperado.items():
            exigir(chave in atual, f"Campo remoto ausente: {caminho}.{chave}")
            conferir_subconjunto(valor, atual[chave], f"{caminho}.{chave}")
    elif isinstance(esperado, list):
        exigir(isinstance(atual, list) and len(esperado) == len(atual), f"Lista divergente: {caminho}")
        # Os arrays do contrato versionado têm ordem estável na API.
        for i, valor in enumerate(esperado):
            conferir_subconjunto(valor, atual[i], f"{caminho}[{i}]")
    else:
        exigir(esperado == atual, f"Valor divergente: {caminho}")


def verificar_github(configs):
    repo = api(f"repos/{REPO}")
    exigir(repo["default_branch"] == "main", "Branch padrão mudou; revise o contrato")
    exigir(repo["allow_auto_merge"] is False, "Auto-merge está habilitado")
    remotos = api(f"repos/{REPO}/rulesets")
    for nome, esperado in configs.items():
        candidatos = [r for r in remotos if r["name"] == nome]
        exigir(len(candidatos) == 1, f"Ruleset remoto ausente ou duplicado: {nome}")
        atual = api(f"repos/{REPO}/rulesets/{candidatos[0]['id']}")
        conferir_subconjunto(esperado, atual, nome)
        print(f"Ruleset {nome}: ativo e compatível (ID {atual['id']})")
    colaboradores = api(f"repos/{REPO}/collaborators?per_page=100")
    exigir(len(colaboradores) < 100, "Paginação de colaboradores necessária; audite antes de prosseguir")
    escrita = {c["login"] for c in colaboradores if c.get("permissions", {}).get("push")}
    exigir(escrita == {"rfdornelles"}, "Permissões de escrita divergem da política do mantenedor único")
    efetivas = api(f"repos/{REPO}/rules/branches/main")
    exigir(any(r["type"] == "required_status_checks" for r in efetivas), "CI não se aplica à main")
    exigir(any(r["type"] == "pull_request" for r in efetivas), "PR não é obrigatório na main")
    print("Main com PR e CI obrigatórios; auto-merge desativado; escrita limitada ao mantenedor.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--github", action="store_true", help="Também auditar configuração remota (somente leitura)")
    args = parser.parse_args()
    configs = ler_configuracoes()
    verificar_links()
    print("Configurações locais, nomes dos checks e links dos guias válidos.")
    if args.github:
        verificar_github(configs)


if __name__ == "__main__":
    try:
        main()
    except (ValueError, KeyError, StopIteration, OSError, subprocess.CalledProcessError) as exc:
        print(f"Auditoria falhou: {exc}", file=sys.stderr)
        sys.exit(1)
