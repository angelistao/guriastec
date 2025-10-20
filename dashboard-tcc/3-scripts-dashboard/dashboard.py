import streamlit as st
import pandas as pd
import plotly.express as px
import os

# configurando a página
st.set_page_config(
    page_title="Dashboard de Análise de Água da Chuva",
    page_icon="💧",
    layout="wide"
)

# título do dashboard
st.title("💧 Análise Interativa de Sistemas de Aproveitamento de Água da Chuva")
st.markdown("Dashboard baseado nos dados de simulação do TCC de Laisa Cancian.")


# caminho relativo para o arquivo de dados na pasta de resultados
caminho_dados = os.path.join('..', '4-dados-resultados', 'Resultado_limpo.csv')

try:
    df = pd.read_csv('dashboard-tcc/4-dados-resultados/Resultado_limpo.csv')
except FileNotFoundError:
    st.error(f"ARQUIVO NÃO ENCONTRADO: '{caminho_dados}'")
    st.error("Verifique se a sua estrutura de pastas está correta e se o arquivo 'Resultado_limpo.csv' existe.")
    st.stop() # Interrompe a execução se o arquivo não for encontrado

# filtro na barra lateral
st.sidebar.header("Filtros")

# '' cidade
lista_cidades = sorted(df['Cidade'].unique().tolist())
cidade_selecionada = st.sidebar.selectbox("Selecione a Cidade:", options=lista_cidades)

# '' filtro de Demanda
lista_demandas = sorted(df['Demanda_L_dia'].unique().astype(int).tolist())
demanda_selecionada = st.sidebar.radio("Selecione a Demanda (L/dia):", options=lista_demandas, horizontal=True)

# '' filtro de Área de Captação
lista_areas = sorted(df['Area_Captacao_m2'].unique().astype(int).tolist())
areas_selecionadas = st.sidebar.multiselect("Selecione as Áreas (m²):", options=lista_areas, default=lista_areas)

# filtragem e exibição
df_filtrado = df[
    (df['Cidade'] == cidade_selecionada) &
    (df['Demanda_L_dia'] == demanda_selecionada) &
    (df['Area_Captacao_m2'].isin(areas_selecionadas))
]

st.subheader(f"Resultados para: {cidade_selecionada} (Demanda de {demanda_selecionada} L/dia)")

if not df_filtrado.empty:
    fig = px.line(
        df_filtrado,
        x='Volume_Reservatorio_L',
        y='Eficiencia_Global_Percentual',
        color='Area_Captacao_m2',
        markers=True,
        labels={
            "Volume_Reservatorio_L": "Volume do Reservatório (L)",
            "Eficiencia_Global_Percentual": "Eficiência Global (%)",
            "Area_Captacao_m2": "Área de Captação (m²)"
        },
        title="Eficiência vs. Volume do Reservatório"
    )
    fig.update_layout(legend_title_text='Área (m²)', yaxis=dict(range=[0, 101]))
    st.plotly_chart(fig, use_container_width=True)
else:
    st.warning("Nenhum dado encontrado para os filtros selecionados.")