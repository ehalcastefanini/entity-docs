<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um formulário para gerenciar informações de transportadoras (carriers). Ele permite que os usuários insiram, editem e visualizem dados relacionados a transportadoras, como código, nome, país, idioma, moeda, pagamento e observações. O objetivo é fornecer uma interface para manipular esses dados de forma eficiente e integrada com serviços externos.

* **Tecnologias Utilizadas:**
  - Delphi (VCL Framework).
  - Componentes personalizados como `TFRAMEFindEditSOA`, `TFRAMEstatusInfo`, e `TsDBEdit`.
  - Serviços SOAP para integração com APIs externas.

* **Tipo de Formulário:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - `EDTcode` (Campo de texto, obrigatório).
      - `EDTabbrName` (Campo de texto).
      - `EDTname` (Campo de texto, obrigatório).
      - `EDTremarks1`, `EDTremarks2`, `EDTremarks3` (Campos de texto para observações).
      - `FRAMEfindCountry`, `FRAMEfindLanguage`, `FRAMEfindCurrency`, `FRAMEfindPayment` (Componentes de busca relacionados a país, idioma, moeda e pagamento).
    - **Ações do Formulário e seus Efeitos:**
      - Alteração de valores nos campos dispara eventos para validação e atualização de dados.
      - Integração com serviços externos para buscar informações relacionadas.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Permitir que o usuário insira ou edite informações de transportadoras.
  - Validar os dados inseridos.
  - Buscar informações relacionadas (país, idioma, moeda, pagamento) através de serviços externos.

* **Componentes Principais:**
  - `TFRAMEFindEditSOA`: Componente para busca de dados externos.
  - `TsDBEdit`: Campos de edição vinculados a um banco de dados.
  - `TFRAMEstatusInfo`: Exibe informações de status.

* **Tradução para Pseudo-código:**
  - Evento `OnChange` de `EDTcode`: `se valor do campo mudar então validar código`.
  - Evento `OnInitializeData`: `se inicializar dados então configurar valores padrão`.
  - Evento `AfterApplyChanges`: `se alterações aplicadas então atualizar interface`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`Create`):
     - Configurações iniciais, como visibilidade de painéis e ações disponíveis.
     - Configuração de serviços e componentes de busca.
  2. Interação do Usuário:
     - Usuário insere ou edita dados nos campos.
     - Eventos são disparados para validação e integração com serviços externos.
  3. Salvamento:
     - Dados são validados e enviados para o serviço correspondente.

* **Dados Necessários:**
  - Código da transportadora.
  - Nome da transportadora.
  - País, idioma, moeda e pagamento (opcionais).
  - Observações (opcionais).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O botão "Salvar" só deve ser habilitado se os campos obrigatórios (`EDTcode` e `EDTname`) estiverem preenchidos.

* **Filtros Disponíveis:**
  - Busca por país, idioma, moeda e pagamento.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se `EDTcode` ou `EDTname` estiver vazio.
  - "Código inválido" se o código não atender aos critérios de validação.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - `EDTcode`: Deve ser único e validado.
  - `EDTname`: Deve ter um valor não vazio.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `m_SetFindCountry`: Configura o componente de busca para países.
  - `m_SetFindLanguage`: Configura o componente de busca para idiomas.
  - `m_SetFindPayment`: Configura o componente de busca para pagamentos.
  - `m_SetFindCurrency`: Configura o componente de busca para moedas.
  - `m_InitializeData`: Inicializa os dados do formulário.
  - `m_AfterApplyChanges`: Atualiza a interface após salvar alterações.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `CarrierServiceUtils`.
  - Endpoint: Não especificado no código.
  - Dados Enviados: Dados da transportadora.
  - Dados Recebidos: Confirmação de sucesso ou erro.
  - Tratamento de Erros: Mensagens de erro exibidas ao usuário.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `InvokeRegistry`, `SOAPHTTPClient`: Para integração com serviços SOAP.
  - `kneFRCtrlEditSOA`, `kneFRFindEditSOA`: Componentes personalizados para edição e busca.

* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA`, `TFRAMEstatusInfo`.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `EDTcode` (string, obrigatório).
  - `EDTabbrName` (string, opcional).
  - `EDTname` (string, obrigatório).
  - `EDTremarks1`, `EDTremarks2`, `EDTremarks3` (string, opcional).
  - `FRAMEfindCountry`, `FRAMEfindLanguage`, `FRAMEfindCurrency`, `FRAMEfindPayment` (componentes de busca).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEcarrier := TFRAMEcarrier.Create(Self);
  FRAMEcarrier.EDTcode.Text := '123';
  FRAMEcarrier.EDTname.Text := 'Transportadora Exemplo';
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="font-family: Tahoma; color: #4D4D4D;">
    <label>Carrier Cd:</label> <input type="text" id="EDTcode" /><br />
    <label>Name:</label> <input type="text" id="EDTname" /><br />
    <label>Country:</label> <input type="text" id="FRAMEfindCountry" /><br />
    <label>Language:</label> <input type="text" id="FRAMEfindLanguage" /><br />
    <label>Currency:</label> <input type="text" id="FRAMEfindCurrency" /><br />
    <label>Payment:</label> <input type="text" id="FRAMEfindPayment" /><br />
    <label>Remarks:</label> <input type="text" id="EDTremarks1" /><br />
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial do formulário no construtor `Create`.
* Métodos para configurar os componentes de busca (`m_SetFindCountry`, etc.).

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar dados de transportadoras, com integração a serviços externos. No entanto, faltam detalhes sobre validações específicas e endpoints de serviços. A modularidade e reutilização de componentes são pontos fortes.

---

## 13. Resumo Curto:

O código implementa um formulário para gerenciar transportadoras, permitindo edição e integração com serviços externos. Ele utiliza componentes personalizados e validações básicas para garantir a consistência dos dados.#### **FRcarrier.pas**

```
unit FRcarrier;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRFindEditSOA, kneFRStatusInfo, kneFRGridEditSOA, sDBEdit, sLabel,
  sFrameAdapter, sBitBtn, sPanel;

type
  TFRAMEcarrier = class(TFRAMEBaseCtrlEditSOA)
    FRAMEfindCountry: TFRAMEFindEditSOA;
    FRAMEfindPayment: TFRAMEFindEditSOA;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    FRAMEfindLanguage: TFRAMEFindEditSOA;
    FRAMEfindCurrency: TFRAMEFindEditSOA;
    LBLcarrierCode: TsLabel;
    Label1: TsLabel;
    Label3: TsLabel;
    Label4: TsLabel;
    Label5: TsLabel;
    Label6: TsLabel;
    Label7: TsLabel;
    Label8: TsLabel;
    Label2: TsLabel;
    EDTcode: TsDBEdit;
    EDTabbrName: TsDBEdit;
    EDTname: TsDBEdit;
    EDTremarks1: TsDBEdit;
    EDTremarks2: TsDBEdit;
    EDTremarks3: TsDBEdit;
    EDTlegalNum: TsDBEdit;
    procedure EDTcodeChange(Sender: TObject);
  private
    { Private declarations }
    mv_KeyInitVal: string;
    FCodeChanged: TNotifyEvent;
    procedure m_SetFindCountry;
    procedure m_SetFindLanguage;
    procedure m_SetFindPayment;
    procedure m_SetFindCurrency;
    procedure m_InitializeData(Sender: TDataSet);
    procedure m_AfterApplyChanges(Sender: TObject);
    procedure SetCodeChanged(const Value: TNotifyEvent);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property CodeChanged: TNotifyEvent read FCodeChanged write SetCodeChanged;

  end;

var
  FRAMEcarrier: TFRAMEcarrier;

implementation

uses
  kneInterfaces, kneFindDialogSOA, kneUtils, kneTypes,
  CarrierServiceUtils, CountryServiceUtils, LanguageServiceUtils, 
  CurrencyServiceUtils, PaymentCarrServiceUtils, MaddressAndContact;

{$R *.dfm}

{ TFRAMEcarrier }

constructor TFRAMEcarrier.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';          // n�o necessita de estar def. na frame Master
  DataPacketName := 'Carrier';
  PropertyName := '';             // n�o necessita de estar def. na frame Master
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCarrierServiceUtils.Create(self);

  //Configura��o do findEdit para o Consignee, Warehouse e Country
  m_SetFindCountry;
  m_SetFindLanguage;
  m_SetFindPayment;
  m_SetFindCurrency;

  // Atribui��o do evento de inicializa��o dos dados
  OnInitializeData := m_InitializeData;
  // Atribui��o do evento do After Apply
  AfterApplyChanges := m_AfterApplyChanges;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
```

#### **FRcarrier.dfm**

```
inherited FRAMEcarrier: TFRAMEcarrier
  object LBLcarrierCode: TsLabel [0]
    Left = 8
    Top = 13
    Width = 53
    Height = 13
    Caption = 'Ca&rrier Cd:'
    FocusControl = EDTcode
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label1: TsLabel [1]
    Left = 8
    Top = 39
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = EDTname
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label3: TsLabel [2]
    Left = 412
    Top = 65
    Width = 51
    Height = 13
    Caption = 'Lan&guage:'
    FocusControl = FRAMEfindLanguage.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label4: TsLabel [3]
    Left = 8
    Top = 65
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
  object Label5: TsLabel [4]
    Left = 412
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
  object Label6: TsLabel [5]
    Left = 8
    Top = 91
    Width = 46
    Height = 13
    Caption = 'Pay&ment:'
    FocusControl = FRAMEfindPayment.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label7: TsLabel [6]
    Left = 8
    Top = 117
    Width = 45
    Height = 13
    Caption = 'Remar&ks:'
    FocusControl = EDTremarks1
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label8: TsLabel [7]
```
<!-- tabs:end -->

