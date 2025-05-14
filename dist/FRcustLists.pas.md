<!-- tabs:start -->

#### **Documentation**

# Documentação do Código `FRcustLists`

## 1. Visão Geral:

### Objetivo Principal:
O objetivo principal do código `FRcustLists` é criar uma interface de formulário para gerenciar listas de clientes (Customer Lists). Ele permite que os usuários visualizem, editem e configurem informações relacionadas a listas de clientes, como código da lista, referência, observações, mercado e unidade de negócios. Este formulário é parte de um sistema maior que utiliza serviços SOAP para manipular dados relacionados a listas de clientes.

### Tecnologias Utilizadas:
- **Delphi**: Linguagem de programação utilizada para criar a interface e lógica do formulário.
- **Componentes Visuais**: `TsLabel`, `TsDBEdit`, `TFRAMEstatusInfo`, `TFRAMEFindEditSOA`, `TFRAMEBusUnit`.
- **Serviços SOAP**: Utilizados para comunicação com serviços externos, como `CustomerListServiceUtils` e `CustomerMarketServiceUtils`.
- **Banco de Dados**: Conexão com banco de dados via `DataSource` e `DataSet`.

### Tipo de Formulário:
Este é um **formulário** com os seguintes elementos:
- **Elementos do Formulário**:
  - `EDTcustListCd` (Código da Lista de Clientes - Campo de texto).
  - `EDTreference` (Referência - Campo de texto).
  - `EDTremarks` (Observações - Campo de texto).
  - `FRAMEFindMarket` (Mercado - Campo de busca).
  - `FRAMEBusUnit1` (Unidade de Negócios - Campo de seleção).
- **Ações do Formulário**:
  - Inicialização de dados.
  - Configuração de mercado.
  - Bloqueio de edição de campos em determinados estados.

---

## 2. Descrição da Funcionalidade:

### Ações Disponíveis:
- Inicializar os dados do formulário.
- Configurar o mercado associado à lista de clientes.
- Bloquear ou permitir a edição de campos com base no estado do formulário.

### Componentes Principais:
- **`TFRAMEstatusInfo`**: Exibe informações de status relacionadas ao formulário.
- **`TFRAMEFindEditSOA`**: Permite buscar e selecionar mercados.
- **`TFRAMEBusUnit`**: Gerencia a seleção de unidades de negócios.
- **`EDTcustListCd`, `EDTreference`, `EDTremarks`**: Campos de entrada de texto para informações específicas.

### Pseudo-código:
- Evento `OnClick` de um botão: `se botão clicado então executar função`.
- Evento `OnChange` de um campo: `se valor do campo alterado então validar campo`.
- Inicialização de dados: `ao inicializar o formulário, configurar fontes de dados e propriedades`.

---

## 3. Lógica Operacional:

### Fluxo de Execução:
1. **Inicialização**:
   - O formulário é criado e inicializado no construtor `Create`.
   - Propriedades como `MasterSource`, `DataPacketName` e `ProviderService` são configuradas.
   - Componentes como `FRAMEstatusInfo1` e `FRAMEBusUnit1` são associados a fontes de dados.

2. **Interações do Usuário**:
   - O usuário pode preencher os campos de texto ou selecionar valores nos componentes de busca e seleção.
   - Alterações nos campos podem disparar eventos para validação ou atualização de dados.

### Dados Necessários:
- Código da Lista de Clientes.
- Referência.
- Observações.
- Mercado.
- Unidade de Negócios.

---

## 4. Regras de Negócio:

### Ações e Pré-condições:
- **Bloqueio de Edição**: O campo `EDTcustListCd` é bloqueado para edição em determinados estados (ex.: ao criar uma nova lista).
- **Configuração de Mercado**: O mercado é configurado automaticamente ao inicializar o formulário.

### Filtros Disponíveis:
- Não há filtros explícitos definidos no código.

### Mensagens de Erro:
- Não há mensagens de erro explícitas definidas no código.

### Valores Padrão dos Campos:
- Não há valores padrão explícitos definidos no código.

### Validações e Condições dos Campos:
- Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

### Funções e Lógica:
1. **`Create`**:
   - Inicializa o formulário e configura propriedades e componentes.
2. **`SetKeyEditing`**:
   - Bloqueia a edição de campos específicos.
3. **`m_SetFindMarket`**:
   - Configura o componente de busca de mercado.

---

## 6. Consumo de Serviços API:

### Chamadas a Serviços Externos:
- **Serviço**: `CustomerListServiceUtils`.
  - **Endpoint**: Não especificado no código.
  - **Dados Enviados**: Não especificado no código.
  - **Dados Recebidos**: Não especificado no código.
  - **Propósito**: Gerenciar listas de clientes.
- **Serviço**: `CustomerMarketServiceUtils`.
  - **Endpoint**: Não especificado no código.
  - **Dados Enviados**: Não especificado no código.
  - **Dados Recebidos**: Não especificado no código.
  - **Propósito**: Gerenciar mercados associados.

---

## 7. Campos Condicionais (Lógica do Formulário):

- Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

### Bibliotecas Externas:
- `kneFRCtrlEditSOA`, `kneFRFindEditSOA`, `kneFRStatusInfo`, `kneFRBusUnit`: Componentes personalizados para gerenciamento de formulários e dados.

### Componentes Personalizados:
- `TFRAMEstatusInfo`, `TFRAMEFindEditSOA`, `TFRAMEBusUnit`.

---

## 9. Listagem de Campos e Validações:

### Campos:
- **`EDTcustListCd`**: Tipo: string, obrigatório, sem validações explícitas.
- **`EDTreference`**: Tipo: string, opcional, sem validações explícitas.
- **`EDTremarks`**: Tipo: string, opcional, sem validações explícitas.
- **`FRAMEFindMarket`**: Tipo: busca, obrigatório, sem validações explícitas.
- **`FRAMEBusUnit1`**: Tipo: seleção, obrigatório, sem validações explícitas.

### Mapeamento:
- Não especificado no código.

---

## 10. Exemplos e Diagramas:

### Fluxograma:
Não aplicável.

### Diagrama de Sequência:
Não aplicável.

### Exemplos de Código:
```delphi
// Inicialização do formulário
FRAMEcustLists := TFRAMEcustLists.Create(Self);
FRAMEcustLists.Show;
```

### Representação HTML:
```html
<div style="font-family: Verdana;">
  <label for="custListCd">Cust List Cd:</label>
  <input type="text" id="custListCd" disabled>
  <label for="reference">Reference:</label>
  <input type="text" id="reference">
  <label for="remarks">Remarks:</label>
  <input type="text" id="remarks">
  <label for="market">Market:</label>
  <input type="text" id="market">
  <label for="busUnit">Business Unit:</label>
  <select id="busUnit">
    <option>Unit 1</option>
    <option>Unit 2</option>
  </select>
</div>
```

---

## 11. Comentários Importantes no Código:

- Configuração de propriedades no construtor `Create`.
- Bloqueio de edição no método `SetKeyEditing`.

---

## 12. Conclusão:

O código `FRcustLists` é um formulário bem estruturado para gerenciar listas de clientes, com integração a serviços SOAP e componentes personalizados. No entanto, faltam validações explícitas, mensagens de erro e valores padrão para os campos, o que pode limitar sua robustez.

---

## 13. Resumo Curto:

O código `FRcustLists` implementa um formulário para gerenciar listas de clientes, integrando serviços SOAP e componentes personalizados. Ele permite configurar mercados, unidades de negócios e informações básicas da lista, mas carece de validações e mensagens de erro explícitas.#### **FRcustLists.pas**

```
unit FRcustLists;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRFindEditSOA, Mask, DBCtrls, sDBEdit, sLabel,
  kneFRStatusInfo, kneFRBusUnit;

type
  TFRAMEcustLists = class(TFRAMEBaseCtrlEditSOA)
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBL2: TsLabel;
    LBL3: TsLabel;
    LBLemail: TsLabel;
    FRAMEFindMarket: TFRAMEFindEditSOA;
    EDTreference: TsDBEdit;
    EDTremarks: TsDBEdit;
    LBL1: TsLabel;
    EDTcustListCd: TsDBEdit;
    LBLbusUnit: TsLabel;
    FRAMEBusUnit1: TFRAMEBusUnit;
  private
    FOnChangeMkt: TNotifyEvent;
    FMktValue: String;
    procedure m_SetFindMarket;
    procedure m_InitializeData(Sender: TDataSet);
    procedure SetOnChangeMkt(const Value: TNotifyEvent);
    procedure m_AfterSetFindMarket(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetKeyEditing(const EditKey: Boolean); override;

    property OnChangeMkt: TNotifyEvent read FOnChangeMkt write SetOnChangeMkt;
  end;

var
  FRAMEcustLists: TFRAMEcustLists;

implementation

uses
  kneTypes, kneUtils, kneFREditSOA, Global, 
  //---
  CustomerListServiceUtils, CustomerMarketServiceUtils;

{$R *.dfm}

{ TFRAMEcustLists }

constructor TFRAMEcustLists.Create(AOwner: TComponent);
begin
  inherited;
  
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'CustomerList';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCustomerListServiceUtils.Create(self);

  m_SetFindMarket;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

  FRAMEBusUnit1.DataSource     := DStable;
  FRAMEBusUnit1.BusUnitList    := gv_BusUnitList;
  FRAMEBusUnit1.BusUnitDefault := gv_DefaultBusUnit;

  OnInitializeData := m_InitializeData;

end;


procedure TFRAMEcustLists.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
  // impedir acesso ao campo (em NEW)
  TkneControls.SetControlState(EDTcustListCd, False);

  FMktValue := FRAMEFindMarket.Text;
end;


procedure TFRAMEcustLists.m_SetFindMarket;
begin
```

#### **FRcustLists.dfm**

```
inherited FRAMEcustLists: TFRAMEcustLists
  Font.Name = 'Verdana'
  object LBL2: TsLabel [0]
    Left = 8
    Top = 65
    Width = 44
    Height = 13
    Caption = 'Market:'
    FocusControl = FRAMEFindMarket.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL3: TsLabel [1]
    Left = 8
    Top = 39
    Width = 63
    Height = 13
    Caption = 'Reference:'
    FocusControl = EDTreference
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLemail: TsLabel [2]
    Left = 8
    Top = 91
    Width = 56
    Height = 13
    Caption = 'Remarks:'
    FocusControl = EDTremarks
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL1: TsLabel [3]
    Left = 8
    Top = 13
    Width = 74
    Height = 13
    Caption = 'Cust List Cd:'
    FocusControl = EDTcustListCd
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLbusUnit: TsLabel [4]
    Left = 243
    Top = 13
    Width = 81
    Height = 13
    Caption = 'Business Unit:'
    FocusControl = FRAMEBusUnit1.DBCBObusUnit
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 215
    Width = 571
    TabOrder = 5
    Visible = False
  end
  inline FRAMEstatusInfo1: TFRAMEstatusInfo [6]
    Left = 0
    Top = 173
    Width = 571
    Height = 42
    Align = alBottom
    AutoScroll = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 4
    inherited GRPstatus: TsGroupBox
      Width = 571
      Font.Name = 'Verdana'
      ParentFont = False
      inherited DBTXTlastUpd: TsDBText
        Width = 129
        Font.Color = 5059883
```
<!-- tabs:end -->

