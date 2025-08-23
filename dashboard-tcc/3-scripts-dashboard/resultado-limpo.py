import pandas as pd
import os

print("Iniciando o script de limpeza...")

# Caminhos relativos para os arquivos
caminho_bruto = os.path.join('..', '4-dados-resultados', 'Resultado.csv')
caminho_limpo = os.path.join('..', '4-dados-resultados', 'Resultado_limpo.csv')

try:
    # Lê o arquivo original, pulando o cabeçalho problemático (header=None)
    # e definindo os nomes corretos das colunas manualmente.
    df = pd.read_csv(
        caminho_bruto,
        delimiter=';',
        header=None,  # Ignora o cabeçalho do arquivo
        skiprows=1,   # Pula a primeira linha que é o cabeçalho
        names=[
            'Cidade',
            'Demanda_L_dia',
            'Area_Captacao_m2',
            'Volume_Reservatorio_L',
            'Eficiencia_Global_Percentual'
        ]
    )

    # Converte colunas que deveriam ser numéricas
    for col in ['Demanda_L_dia', 'Area_Captacao_m2', 'Volume_Reservatorio_L', 'Eficiencia_Global_Percentual']:
        df[col] = pd.to_numeric(df[col], errors='coerce')

    # Remove linhas que possam ter tido erro de conversão
    df.dropna(inplace=True)

    # Salva o arquivo limpo
    df.to_csv(caminho_limpo, index=False)

    print(f"Arquivo '{caminho_limpo}' criado/atualizado com sucesso!")
    print("Visualização das 5 primeiras linhas dos dados corrigidos:")
    print(df.head())

except FileNotFoundError:
    print(f"Erro: Arquivo '{caminho_bruto}' não encontrado. Verifique a estrutura das pastas.")
except Exception as e:
    print(f"Ocorreu um erro inesperado: {e}")