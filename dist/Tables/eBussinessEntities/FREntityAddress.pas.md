<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um formulário para gerenciar endereços de entidades. Ele permite que os usuários insiram, editem e validem informações relacionadas a endereços, como endereço principal, endereço secundário, cidade, código postal, descrição do endereço e país. Este formulário é utilizado como parte de um sistema maior para gerenciar dados de entidades.

* **Tecnologias Utilizadas:**  
  - Delphi (VCL - Visual Component Library).
  - Componentes personalizados como `TsLabel`, `TsDBEdit`, e `TFRAMEFindEditSOA`.
  - Serviços SOAP para integração com dados externos.

* **Tipo de Formulário:**  
  Este é um formulário com os seguintes elementos:
  - **Elementos do Formulário e Tipos:**
    - `EDTaddress` (Campo de texto para endereço principal).
    - `EDTaddress2` (Campo de texto para endereço secundário).
    - `EDTaddress3` (Campo de texto para endereço terciário).
    - `EDTcity` (Campo de texto para cidade).
    - `EDTpostDescript` (Campo de texto para descrição do endereço).
    - `FRAMEfindCountry` (Campo de busca para selecionar o país).
    - `EDTpostCode` (Campo de texto para código postal).
  - **Ações do Formulário e Efeitos:**
    - Validação de dados ao salvar.
    - Inicialização de dados ao carregar o formulário.
    - Configuração de propriedades específicas para os campos.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Inserir novos registros de endereço.
  - Editar registros existentes.
  - Validar os dados inseridos.
  - Salvar alterações pendentes.

* **Componentes Principais:**
  - `TFRAMEentityAddress`: Classe principal que gerencia o formulário.
  - `TsLabel`: Rótulos para identificar os campos.
  - `TsDBEdit`: Campos de entrada de texto vinculados a um banco de dados.
  - `TFRAMEFindEditSOA`: Componente para busca de países.
  - `TFRAMEstatusInfo`: Exibe informações de status relacionadas ao formulário.

* **Pseudo-código de Ações e Eventos:**
  - `OnNewRecord` do dataset: `se novo registro for criado, inicializar valores padrão`.
  - `OnBeforeInsert` do dataset: `se antes de inserir, verificar condições`.
  - `OnDataChange` do dataset: `se dados forem alterados, atualizar status`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`Create`):
     - Configura propriedades como `MasterSource`, `DataPacketName`, e `FrameType`.
     - Configura visibilidade de painéis e ações disponíveis.
     - Atribui eventos como `OnInitializeData`.
  2. Interação do Usuário:
     - Usuário preenche os campos do formulário.
     - Ao salvar, o método `ApplyChanges` é chamado para validar e salvar os dados.
  3. Funções Executadas:
     - `ApplyChanges` (Arquivo: `FRentityAddress.pas`): Salva alterações pendentes.
     - `m_SetFindCounty` (Arquivo: `FRentityAddress.pas`): Configura o campo de busca de país.
     - `m_InitializeData` (Arquivo: `FRentityAddress.pas`): Inicializa os dados do formulário.

* **Dados Necessários:**
  - Endereço principal, cidade, código postal, descrição do endereço e país.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação "Salvar": Só pode ser executada se todos os campos obrigatórios forem preenchidos corretamente.

* **Filtros Disponíveis:**
  - Campo de busca para selecionar o país.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Formato inválido" se o valor inserido não atender ao formato esperado.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - `EDTaddress`: Deve conter texto.
  - `EDTpostCode`: Deve conter apenas números.
  - `FRAMEfindCountry`: Deve ser selecionado um país válido.

---

## 5. Funções Principais:

* **`ApplyChanges`:**  
  Salva alterações pendentes no formulário. Não invoca diretamente o serviço de gravação, pois isso é responsabilidade do formulário principal.

* **`m_SetFindCounty`:**  
  Configura o campo de busca para o país.

* **`m_InitializeData`:**  
  Inicializa os dados do formulário ao carregar.

---

## 6. Consumo de Serviços API:

* **Nenhuma chamada direta a serviços externos é feita neste código.**  
  A gravação dos dados é responsabilidade do formulário principal que utiliza este frame.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `InvokeRegistry`, `SOAPHTTPClient`: Para integração com serviços SOAP.
  - `kneUtils`, `kneTypes`: Utilitários personalizados.

* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA`: Campo de busca para seleção de país.
  - `TFRAMEstatusInfo`: Exibe informações de status.

---

## 9. Listagem de Campos e Validações:

* **Campos do Formulário:**
  - `EDTaddress` (tipo: string, obrigatório).
  - `EDTaddress2` (tipo: string, opcional).
  - `EDTaddress3` (tipo: string, opcional).
  - `EDTcity` (tipo: string, obrigatório).
  - `EDTpostDescript` (tipo: string, opcional).
  - `FRAMEfindCountry` (tipo: busca, obrigatório).
  - `EDTpostCode` (tipo: string, obrigatório, apenas números).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não definido explicitamente no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```pascal
  var
    Frame: TFRAMEentityAddress;
  begin
    Frame := TFRAMEentityAddress.Create(Self);
    Frame.Show;
  end;
  ```

* **HTML Representando o Formulário:**
  ```html
  <div style="width: 1033px; height: 522px;">
    <label for="address">Address:</label>
    <input type="text" id="address" style="width: 200px;"><br>
    <label for="address2">Address 2:</label>
    <input type="text" id="address2" style="width: 200px;"><br>
    <label for="address3">Address 3:</label>
    <input type="text" id="address3" style="width: 200px;"><br>
    <label for="city">City:</label>
    <input type="text" id="city" style="width: 200px;"><br>
    <label for="postDescript">Post Descrip.:</label>
    <input type="text" id="postDescript" style="width: 200px;"><br>
    <label for="country">Country:</label>
    <input type="text" id="country" style="width: 200px;"><br>
    <label for="postCode">Post Code:</label>
    <input type="text" id="postCode" style="width: 200px;"><br>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* O método `ApplyChanges` não grava diretamente os dados, delegando essa responsabilidade ao formulário principal.

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar endereços de entidades. Ele é modular e utiliza componentes personalizados para facilitar a integração com outros sistemas. No entanto, a validação e os valores padrão poderiam ser mais detalhados.

---

## 13. Resumo Curto:

Formulário modular em Delphi para gerenciar endereços de entidades, com validação básica e integração com serviços SOAP. Utiliza componentes personalizados para busca e exibição de status.#### **FREntityAddress.pas**

```
unit FRentityAddress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, kneFRStatusInfo,
  kneFRFindEditSOA, StdCtrls, Mask, DBCtrls, sDBEdit, sLabel,
  sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB,
  DBClient, Buttons, sBitBtn, sPanel;

type
  TFRAMEentityAddress = class(TFRAMEBaseCtrlEditSOA)
    LBLentityCode: TsLabel;
    EDTaddress: TsDBEdit;
    LBLaddress2: TsLabel;
    EDTaddress2: TsDBEdit;
    LBLaddress3: TsLabel;
    EDTaddress3: TsDBEdit;
    LBLcity: TsLabel;
    EDTcity: TsDBEdit;
    LBLpostDescript: TsLabel;
    EDTpostDescript: TsDBEdit;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    LBLCounty: TsLabel;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    sLabel1: TsLabel;
    EDTpostCode: TsDBEdit;
    procedure CDStableNewRecord(DataSet: TDataSet);
    procedure CDStableBeforeInsert(DataSet: TDataSet);
    procedure DStableDataChange(Sender: TObject; Field: TField);
  private
    { Private declarations }
    FMinAddrNum: Integer;
    procedure m_SetFindCounty;
    procedure m_InitializeData(DataSet: TDataSet);
    function ApplyChanges(pv_showException : Boolean = True): Boolean;  override;
    function m_Validate: Boolean;  override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);   override;
  end;

var
  FRAMEentityAddress: TFRAMEentityAddress;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes, kneCBEdit, kneFGDBUtils, Global,
  CountryServiceUtils;

constructor TFRAMEentityAddress.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'Address';
  PropertyName := 'address';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService:= nil;

  //Configura��o dos findEdits
  m_SetFindCounty;

  // Atribui��o do evento de inicializa��o dos dados
  OnInitializeData := m_InitializeData;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;
end;

// ################ ApplyChanges  ###########################################
function TFRAMEentityAddress.ApplyChanges(pv_showException : Boolean): Boolean;
begin
  // JAR: 02-01-2008
  // N�o � invocado o servi�o para gravar. A grava��o dos
  // addresses e dos contacts fica a cargo do Form "Master" que chama o form
  // dos addresses e Contacts
  m_SetScrollEventState(False);

  SavePendingChanges;

  m_SetScrollEventState(True);
  Result:= True;
end;

// ################ m_SetFindCounty  ###########################################
```

#### **FREntityAddress.dfm**

```
inherited FRAMEentityAddress: TFRAMEentityAddress
  Width = 1033
  Height = 522
  object LBLentityCode: TsLabel [0]
    Left = 8
    Top = 13
    Width = 43
    Height = 13
    Caption = '&Address:'
    FocusControl = EDTaddress
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLaddress2: TsLabel [1]
    Left = 8
    Top = 39
    Width = 52
    Height = 13
    Caption = 'A&ddress 2:'
    FocusControl = EDTaddress2
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLaddress3: TsLabel [2]
    Left = 8
    Top = 65
    Width = 52
    Height = 13
    Caption = 'Addr&ess 3:'
    FocusControl = EDTaddress3
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLcity: TsLabel [3]
    Left = 8
    Top = 91
    Width = 23
    Height = 13
    Caption = 'C&ity:'
    FocusControl = EDTcity
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLpostDescript: TsLabel [4]
    Left = 8
    Top = 117
    Width = 67
    Height = 13
    Caption = '&Post Descrip.:'
    FocusControl = EDTpostDescript
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLCounty: TsLabel [5]
    Left = 8
    Top = 143
    Width = 43
    Height = 13
    Caption = 'Countr&y:'
    FocusControl = FRAMEfindCountry.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object sLabel1: TsLabel [6]
    Left = 497
    Top = 91
    Width = 53
    Height = 13
    Caption = 'Post Code:'
    FocusControl = EDTcity
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
```
<!-- tabs:end -->

