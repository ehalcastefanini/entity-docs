<!-- tabs:start -->

#### **Documentation**

# Documentação do Código: Unidade `FRbankAccounts`

## 1. Visão Geral:

### Objetivo Principal:
O objetivo principal deste código é criar uma interface gráfica para gerenciar contas bancárias. Ele permite que os usuários visualizem, editem e configurem informações relacionadas a contas bancárias, como ID do banco, nome curto, nome da conta, código SWIFT, moeda, moinho (mill), número da conta e descrição. O código também integra serviços externos para buscar informações adicionais, como moeda, moinho e vendedor.

### Tecnologias Utilizadas:
- **Delphi**: Linguagem de programação utilizada para criar a interface gráfica e lógica do sistema.
- **Componentes Visuais**: `TsLabel`, `TsDBEdit`, `TFRAMEFindEditSOA`, entre outros, para criar a interface do usuário.
- **Serviços SOAP**: Integração com serviços externos para buscar dados relacionados a bancos, moedas, moinhos e vendedores.
- **Banco de Dados**: Utilização de `DataSource` e `DBClient` para manipulação de dados.

### Tipo de Formulário:
Este é um **formulário** com os seguintes elementos:
- **Elementos do Formulário**:
  - `EDTbankID` (Campo de texto para ID do banco).
  - `EDTshortName` (Campo de texto para nome curto do banco).
  - `EDTbankName` (Campo de texto para nome da conta).
  - `EDTswift` (Campo de texto para código SWIFT).
  - `FRAMEfindCurrency` (Campo de busca para moeda).
  - `FRAMEfindMill` (Campo de busca para moinho).
  - `EDTcountNumber` (Campo de texto para número da conta).
  - `EDTdescrip1`, `EDTdescrip2`, `EDTdescrip3` (Campos de texto para descrição).
  - `FRAMEfindSeller` (Campo de busca para vendedor).
- **Ações do Formulário**:
  - Configuração de visibilidade e estado dos campos.
  - Integração com serviços externos para busca de dados.

---

## 2. Descrição da Funcionalidade:

### Ações Específicas:
- Os usuários podem preencher ou editar informações sobre contas bancárias.
- O sistema busca automaticamente informações adicionais (como moeda, moinho e vendedor) através de serviços externos.
- Validações são realizadas ao sair de campos específicos, como `EDTshortName`.

### Componentes Principais:
- **Labels (`TsLabel`)**: Exibem descrições para os campos.
- **Campos de Texto (`TsDBEdit`)**: Permitem entrada de dados.
- **Componentes de Busca (`TFRAMEFindEditSOA`)**: Integram-se com serviços externos para buscar dados.
- **FRAMEstatusInfo**: Exibe informações de status relacionadas ao banco de dados.

### Pseudo-código:
- Evento `OnExit` do campo `EDTshortName`:
  ```
  se o valor do campo EDTshortName for alterado então
    validar o valor do campo
  ```
- Configuração de busca para moeda:
  ```
  configurar FRAMEfindCurrency para buscar dados de moeda
  ```
- Configuração de busca para moinho:
  ```
  configurar FRAMEfindMill para buscar dados de moinho
  ```

---

## 3. Lógica Operacional:

### Fluxo de Execução:
1. **Inicialização**:
   - O formulário é carregado e os componentes são configurados.
   - Serviços externos são configurados para busca de dados.
2. **Interações do Usuário**:
   - O usuário preenche os campos ou utiliza os componentes de busca.
   - Eventos são disparados ao sair de campos ou ao realizar buscas.
3. **Funções Executadas**:
   - `m_SetFindCurrency`: Configura o componente de busca para moeda.
   - `m_SetFindMill`: Configura o componente de busca para moinho.
   - `m_SetFindSeller`: Configura o componente de busca para vendedor.

### Dados Necessários:
- ID do banco, nome curto, nome da conta, código SWIFT, moeda, moinho, número da conta, descrição e vendedor.

---

## 4. Regras de Negócio:

### Ações e Pré-condições:
- **Ação**: Buscar moeda.
  - **Pré-condição**: O campo de busca deve estar configurado.
- **Ação**: Salvar dados.
  - **Pré-condição**: Todos os campos obrigatórios devem estar preenchidos.

### Filtros Disponíveis:
- Não há filtros explícitos definidos no código.

### Mensagens de Erro:
- Não há mensagens de erro explícitas definidas no código.

### Valores Padrão dos Campos:
- Não há valores padrão explícitos definidos no código.

### Validações e Condições dos Campos:
- Validações específicas não estão definidas no código.

---

## 5. Funções Principais:

- **`Create`**:
  - Configura o formulário e inicializa os componentes.
- **`m_SetFindCurrency`**:
  - Configura o componente de busca para moeda.
- **`m_SetFindMill`**:
  - Configura o componente de busca para moinho.
- **`m_SetFindSeller`**:
  - Configura o componente de busca para vendedor.

---

## 6. Consumo de Serviços API:

- **Serviço**: `BankServiceUtils`
  - **Endpoint**: Não especificado.
  - **Dados Enviados**: Não especificado.
  - **Dados Recebidos**: Não especificado.
  - **Propósito**: Gerenciar dados de bancos.

---

## 7. Campos Condicionais (Lógica do Formulário):

- Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

### Bibliotecas Externas:
- `kneTypes`, `Global`, `kneUtils`: Utilizadas para funcionalidades auxiliares.
- `BankServiceUtils`, `CurrencyServiceUtils`, `MillServiceUtils`, `SellerServiceUtils`: Integração com serviços externos.

### Componentes Personalizados:
- `TFRAMEFindEditSOA`: Componente de busca.
- `TFRAMEstatusInfo`: Exibe informações de status.

---

## 9. Listagem de Campos e Validações:

- **ID do Banco** (`EDTbankID`): Tipo texto, obrigatório.
- **Nome Curto** (`EDTshortName`): Tipo texto, obrigatório.
- **Nome da Conta** (`EDTbankName`): Tipo texto, obrigatório.
- **Código SWIFT** (`EDTswift`): Tipo texto, obrigatório.
- **Moeda** (`FRAMEfindCurrency`): Tipo busca, obrigatório.
- **Moinho** (`FRAMEfindMill`): Tipo busca, obrigatório.
- **Número da Conta** (`EDTcountNumber`): Tipo texto, obrigatório.
- **Descrição** (`EDTdescrip1`, `EDTdescrip2`, `EDTdescrip3`): Tipo texto, opcional.
- **Vendedor** (`FRAMEfindSeller`): Tipo busca, obrigatório.

---

## 10. Exemplos e Diagramas:

### Fluxograma:
Não aplicável.

### Diagrama de Sequência:
Não aplicável.

### Código HTML Representando o Formulário:
```html
<div style="width: 698px; height: 480px;">
  <label style="position: absolute; left: 16px; top: 14px;">Bank ID:</label>
  <input type="text" style="position: absolute; left: 100px; top: 14px;" />
  <label style="position: absolute; left: 16px; top: 158px;">Bank Short Name:</label>
  <input type="text" style="position: absolute; left: 100px; top: 158px;" />
  <label style="position: absolute; left: 16px; top: 186px;">Account Name:</label>
  <input type="text" style="position: absolute; left: 100px; top: 186px;" />
  <!-- Outros campos omitidos para brevidade -->
</div>
```

---

## 11. Comentários Importantes no Código:

- Configuração de propriedades do formulário no construtor `Create`.
- Configuração de visibilidade e estado dos componentes.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar contas bancárias, com integração a serviços externos. No entanto, faltam definições explícitas de validações, mensagens de erro e valores padrão.

---

## 13. Resumo Curto:

Formulário Delphi para gerenciar contas bancárias, com integração a serviços SOAP para buscar dados de moeda, moinho e vendedor. Inclui campos para ID, nome, SWIFT e descrição.#### **FRbankAccounts.pas**

```
unit FRbankAccounts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, StdCtrls, Mask, DBCtrls,
  sDBEdit, kneFRStatusInfo, sLabel, sFrameAdapter, ImgList, ActnList,
  ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, Buttons, sBitBtn, sPanel,
  kneFRFindEditSOA;

type
  TFRAMEbankAccounts = class(TFRAMEBaseCtrlEditSOA)
    LBL1: TsLabel;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    EDTbankID: TsDBEdit;
    LBLshortName: TsLabel;
    EDTshortName: TsDBEdit;
    LBLbankName: TsLabel;
    EDTbankName: TsDBEdit;
    LBLswift: TsLabel;
    EDTswift: TsDBEdit;
    LBLcurrency: TsLabel;
    LBLmill: TsLabel;
    FRAMEfindCurrency: TFRAMEFindEditSOA;
    FRAMEfindMill: TFRAMEFindEditSOA;
    LBLcountNumber: TsLabel;
    EDTcountNumber: TsDBEdit;
    LBLdesc: TsLabel;
    EDTdescrip1: TsDBEdit;
    EDTdescrip2: TsDBEdit;
    EDTdescrip3: TsDBEdit;
    LBLseller: TsLabel;
    FRAMEfindSeller: TFRAMEFindEditSOA;
    procedure EDTshortNameExit(Sender: TObject);
  private
    procedure m_SetFindCurrency;
    procedure m_SetFindMill;
    procedure m_SetFindSeller;

    procedure m_AfterFindMill(Sender: TObject);
    procedure m_AfterFindSeller(Sender: TObject);
    procedure m_SetAccessMode(Sender: TObject; var pv_State: Boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;

var
  FRAMEbankAccounts: TFRAMEbankAccounts;

implementation

uses
  kneTypes, Global, kneUtils,
  //---
  BankServiceUtils, CurrencyServiceUtils, MillServiceUtils, SellerServiceUtils;

{$R *.dfm}

constructor TFRAMEbankAccounts.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'Bank';
  PropertyName := '';
  FrameType := frtMaster;  

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TBankServiceUtils.Create(self);

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;
  OnSetAccessMode := m_SetAccessMode;

  // Finds
  m_SetFindCurrency;
  m_SetFindMill;
  m_SetFindSeller;
end;


procedure TFRAMEbankAccounts.m_SetAccessMode(Sender: TObject; var pv_State: Boolean);
var b_enabled : Boolean;
begin

  TkneControls.SetControlState(EDTbankID, False);
end;

```

#### **FRbankAccounts.dfm**

```
inherited FRAMEbankAccounts: TFRAMEbankAccounts
  Width = 698
  Height = 480
  object LBL1: TsLabel [0]
    Left = 16
    Top = 14
    Width = 41
    Height = 13
    Caption = 'Bank ID:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLshortName: TsLabel [1]
    Left = 16
    Top = 158
    Width = 86
    Height = 13
    Caption = 'Bank Short Name:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLbankName: TsLabel [2]
    Left = 16
    Top = 186
    Width = 73
    Height = 13
    Caption = 'Account Name:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLswift: TsLabel [3]
    Left = 16
    Top = 130
    Width = 28
    Height = 13
    Caption = 'Swift:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLcurrency: TsLabel [4]
    Left = 16
    Top = 101
    Width = 48
    Height = 13
    Caption = 'Currency:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLmill: TsLabel [5]
    Left = 16
    Top = 41
    Width = 18
    Height = 13
    Caption = 'Mill:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLcountNumber: TsLabel [6]
    Left = 16
    Top = 214
    Width = 43
    Height = 13
    Caption = 'Account:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLdesc: TsLabel [7]
    Left = 16
    Top = 242
    Width = 73
    Height = 13
    Caption = 'Account Name:'
```
<!-- tabs:end -->

