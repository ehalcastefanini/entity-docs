<!-- tabs:start -->

#### **Documentation**

# Documentação do Código: Unidade `FRcustMarket`

## 1. Visão Geral:

### Objetivo Principal:
O objetivo principal deste código é criar uma interface gráfica para gerenciar informações de mercados de clientes. Ele permite que os usuários insiram, editem e visualizem dados relacionados a mercados, como código do mercado, descrição, região e moeda. A interface é baseada em um formulário com campos vinculados a um banco de dados.

### Tecnologias Utilizadas:
- **Delphi**: Linguagem de programação utilizada para criar a interface e lógica do formulário.
- **Componentes Visuais**: `TsLabel`, `TsDBEdit`, `TFRAMEFindEditSOA`, `TFRAMEstatusInfo` para criar e gerenciar os elementos da interface.
- **Serviços SOAP**: Utilizados para buscar e manipular dados de regiões e moedas.
- **Banco de Dados**: Conexão com um banco de dados para manipulação de dados através de `DataSource` e `DataField`.

### Tipo de Formulário:
Este é um **formulário** com os seguintes elementos:
- **Elementos do Formulário**:
  - `EDTmarketCode` (Campo de texto para código do mercado).
  - `EDTdescription` (Campo de texto para descrição do mercado).
  - `FRAMEfindRegion` (Campo de busca para selecionar uma região).
  - `FRAMEfindCurrency` (Campo de busca para selecionar uma moeda).
- **Ações do Formulário**:
  - Configuração de serviços para busca de regiões e moedas.
  - Vinculação de campos a um banco de dados.

---

## 2. Descrição da Funcionalidade:

### Ações Disponíveis:
- Inserir ou editar o código do mercado.
- Inserir ou editar a descrição do mercado.
- Selecionar uma região através de um campo de busca.
- Selecionar uma moeda através de um campo de busca.

### Componentes Principais:
1. **`EDTmarketCode`**: Campo de texto para entrada do código do mercado.
2. **`EDTdescription`**: Campo de texto para entrada da descrição do mercado.
3. **`FRAMEfindRegion`**: Componente para busca e seleção de uma região.
4. **`FRAMEfindCurrency`**: Componente para busca e seleção de uma moeda.
5. **`FRAMEstatusInfo1`**: Exibe informações de status relacionadas ao mercado.

### Pseudo-código de Ações e Eventos:
- `Ao clicar no campo de busca de região`: `Abrir diálogo de seleção de região`.
- `Ao clicar no campo de busca de moeda`: `Abrir diálogo de seleção de moeda`.
- `Ao alterar o valor de um campo`: `Validar o valor e atualizar o banco de dados`.

---

## 3. Lógica Operacional:

### Fluxo de Execução:
1. **Inicialização**:
   - O formulário é carregado e os componentes são configurados.
   - Os serviços de busca de região e moeda são configurados.
   - Os campos são vinculados ao banco de dados.

2. **Interações do Usuário**:
   - O usuário insere ou edita os valores nos campos.
   - O usuário utiliza os campos de busca para selecionar uma região ou moeda.

### Dados Necessários:
- Código do mercado.
- Descrição do mercado.
- Região (selecionada via busca).
- Moeda (selecionada via busca).

---

## 4. Regras de Negócio:

### Ações e Pré-condições:
- **Selecionar Região**:
  - Pré-condição: O campo de busca deve estar configurado com o serviço de regiões.
- **Selecionar Moeda**:
  - Pré-condição: O campo de busca deve estar configurado com o serviço de moedas.

### Filtros Disponíveis:
- Não há filtros explícitos definidos no código.

### Mensagens de Erro:
- Não há mensagens de erro explícitas definidas no código.

### Valores Padrão dos Campos:
- Não há valores padrão definidos no código.

### Validações e Condições dos Campos:
- `EDTmarketCode`: Deve ser preenchido em letras maiúsculas.
- `EDTdescription`: Deve ser preenchido em letras maiúsculas.
- Validações adicionais não estão definidas no código.

---

## 5. Funções Principais:

### Funções e Lógica:
1. **`m_SetFindRegion`**:
   - Configura o campo de busca para seleção de regiões.
   - Define o serviço de regiões e os campos de código e descrição.

2. **`m_SetFindCurrency`**:
   - Configura o campo de busca para seleção de moedas.
   - Define o serviço de moedas e os campos de código e descrição.

3. **`Create`**:
   - Inicializa o formulário e configura os componentes e serviços.

---

## 6. Consumo de Serviços API:

### Chamadas a Serviços Externos:
1. **Serviço de Regiões**:
   - Nome do Serviço: `RegionServiceUtils`.
   - Finalidade: Buscar e selecionar regiões.
2. **Serviço de Moedas**:
   - Nome do Serviço: `CurrencyServiceUtils`.
   - Finalidade: Buscar e selecionar moedas.

---

## 7. Campos Condicionais (Lógica do Formulário):

- Não há campos condicionais definidos no código.

---

## 8. Dependências:

### Bibliotecas Externas:
- `InvokeRegistry`, `SOAPHTTPClient`: Para consumo de serviços SOAP.
- `DB`, `DBClient`: Para manipulação de dados do banco de dados.

### Componentes Personalizados:
- `TFRAMEFindEditSOA`: Componente para busca e seleção de dados.
- `TFRAMEstatusInfo`: Componente para exibição de informações de status.

---

## 9. Listagem de Campos e Validações:

### Campos do Formulário:
1. **`EDTmarketCode`**:
   - Tipo: String.
   - Obrigatório: Sim.
   - Validação: Letras maiúsculas.
2. **`EDTdescription`**:
   - Tipo: String.
   - Obrigatório: Sim.
   - Validação: Letras maiúsculas.
3. **`FRAMEfindRegion`**:
   - Tipo: Campo de busca.
   - Obrigatório: Sim.
4. **`FRAMEfindCurrency`**:
   - Tipo: Campo de busca.
   - Obrigatório: Sim.

### Mapeamento de Campos:
- `marketCode` → Coluna no banco de dados: `marketCode`.
- `description` → Coluna no banco de dados: `description`.

---

## 10. Exemplos e Diagramas:

### Fluxograma:
Não aplicável.

### Diagrama de Sequência:
Não aplicável.

### Código HTML Representando o Formulário:
```html
<div style="font-family: Tahoma; font-size: 12px;">
  <label for="marketCode">Code:</label>
  <input id="marketCode" type="text" style="text-transform: uppercase;" />
  <br />
  <label for="description">Description:</label>
  <input id="description" type="text" style="text-transform: uppercase;" />
  <br />
  <label for="region">Region:</label>
  <input id="region" type="text" placeholder="Search Region..." />
  <br />
  <label for="currency">Currency:</label>
  <input id="currency" type="text" placeholder="Search Currency..." />
</div>
```

---

## 11. Comentários Importantes no Código:

- Configuração de serviços para busca de regiões e moedas.
- Vinculação de campos ao banco de dados.

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar informações de mercados de clientes. Ele utiliza serviços SOAP para buscar dados de regiões e moedas, e vincula os campos a um banco de dados. No entanto, faltam validações e mensagens de erro explícitas, o que pode ser uma limitação.

---

## 13. Resumo Curto:

O código implementa um formulário para gerenciar mercados de clientes, com campos vinculados a um banco de dados e integração com serviços SOAP para busca de regiões e moedas. Ele é funcional, mas carece de validações e mensagens de erro explícitas.#### **FRcustMarket.pas**

```
unit FRcustMarket;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRStatusInfo, kneFRFindEditSOA, Mask, DBCtrls,
  sDBEdit, sLabel;

type
  TFRAMEcustMarket = class(TFRAMEBaseCtrlEditSOA)
    LBL1: TsLabel;
    EDTmarketCode: TsDBEdit;
    sLabel1: TsLabel;
    EDTdescription: TsDBEdit;
    sLabel2: TsLabel;
    FRAMEfindRegion: TFRAMEFindEditSOA;
    sLabel3: TsLabel;
    FRAMEfindCurrency: TFRAMEFindEditSOA;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
  private
    { Private declarations }
    procedure m_SetFindRegion;
    procedure m_SetFindCurrency;        
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;    
  end;

var
  FRAMEcustMarket: TFRAMEcustMarket;

implementation

uses
  CustomerMarketServiceUtils, RegionServiceUtils, CurrencyServiceUtils,
  kneTypes;

{$R *.dfm}

{ TFRAMEcustMarket }

constructor TFRAMEcustMarket.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'CustomerMarket';
  PropertyName := '';
  FrameType := frtMaster;  

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCustomerMarketServiceUtils.Create(self);

  m_SetFindRegion;
  m_SetFindCurrency;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

end;

procedure TFRAMEcustMarket.m_SetFindCurrency;
begin
  with FRAMEfindCurrency do
  begin

    with FindDialog do
    begin
      Caption := 'Currency Selection';
      ProviderService := TCurrencyServiceUtils.Create(FindDialog);
    end;

    with FindSettings.DataSelection do
    begin
      UseTargetDataSet := False;
      FieldNameForCode := 'currencyCode';
      FieldNamesForDesc.Clear;
      //FieldNamesForDesc.Add('');
    end;

    with EditSettings do
    begin
      DataSource := DStable;
      FieldNameForCode := 'currency';
      //FieldNameForDesc := '';
    end;
  end;
end;

```

#### **FRcustMarket.dfm**

```
inherited FRAMEcustMarket: TFRAMEcustMarket
  object LBL1: TsLabel [0]
    Left = 8
    Top = 13
    Width = 29
    Height = 13
    Caption = 'C&ode:'
    FocusControl = EDTmarketCode
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object sLabel1: TsLabel [1]
    Left = 8
    Top = 39
    Width = 57
    Height = 13
    Caption = '&Description:'
    FocusControl = EDTdescription
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object sLabel2: TsLabel [2]
    Left = 8
    Top = 65
    Width = 37
    Height = 13
    Caption = '&Region:'
    FocusControl = FRAMEfindRegion.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object sLabel3: TsLabel [3]
    Left = 8
    Top = 91
    Width = 48
    Height = 13
    Caption = 'C&urrency:'
    FocusControl = FRAMEfindCurrency.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 227
    Width = 465
    TabOrder = 5
    Visible = False
  end
  object EDTmarketCode: TsDBEdit [5]
    Left = 80
    Top = 8
    Width = 73
    Height = 21
    AutoSize = False
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'marketCode'
    DataSource = DStable
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object EDTdescription: TsDBEdit [6]
    Left = 80
    Top = 34
    Width = 385
    Height = 21
    AutoSize = False
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'description'
```
<!-- tabs:end -->

