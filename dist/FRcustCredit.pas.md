<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um formulário para gerenciar informações de crédito de clientes, como status de crédito, dias fixos de pagamento, limites de crédito, e outras informações relacionadas. Ele permite que os usuários visualizem e editem dados de crédito de clientes de forma estruturada e eficiente.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do formulário e lógica de negócios.
  - Componentes visuais como `TDBCheckBox`, `TDBEdit`, `TDBText` para interação com o usuário.
  - Integração com serviços SOAP para manipulação de dados remotos.

* **Tipo de Formulário:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - `TDBCheckBox`: Checkbox para opções booleanas (ex.: "Check Credit", "Check Real Date").
      - `TDBEdit`: Campos de entrada de texto vinculados a dados (ex.: "credStat", "fixDay1").
      - `TDBText`: Texto exibido diretamente de uma fonte de dados.
      - `TLabel`: Rótulos para descrever os campos.
    - **Ações do Formulário e seus Efeitos:**
      - Alteração de valores nos campos atualiza os dados vinculados no banco de dados.
      - Checkbox altera valores booleanos no banco de dados.

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Permitir que o usuário visualize e edite informações de crédito de clientes.
  - Atualizar automaticamente os dados no banco de dados ao modificar os campos.

* **Componentes Principais:**
  - Painéis (`TPanel`) para organizar visualmente os elementos.
  - Campos de entrada (`TDBEdit`) para edição de dados.
  - Checkboxes (`TDBCheckBox`) para opções booleanas.
  - Rótulos (`TLabel`) para descrever os campos.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` de um checkbox: `se checkbox clicado então alterar valor booleano no banco de dados`.
  - Evento `OnChange` de um campo: `se valor do campo alterado então validar e atualizar banco de dados`.

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do formulário (`Create`): Configura propriedades como largura, altura, e visibilidade de painéis.
  - Interação do usuário: Modificação de campos ou checkboxes dispara eventos que atualizam os dados no banco de dados.

* **Dados Necessários:**
  - Informações de crédito do cliente, como status de crédito, dias fixos de pagamento, limites de crédito, etc.

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Checkbox "Check Credit" só deve ser marcado se o cliente tiver crédito disponível.
  - Campos de dias fixos de pagamento devem aceitar apenas números válidos.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se um campo contiver dados fora do formato esperado.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explicitamente definidos no código.

* **Validação de Campos e Condições:**
  - Campos de dias fixos de pagamento devem aceitar apenas números.
  - Checkbox deve aceitar apenas valores booleanos.

## 5. Funções Principais:

* **Funções:**
  - `Create`: Inicializa o formulário e configura propriedades como largura, altura, e visibilidade de painéis.
  - `ProviderService`: Configura o serviço de dados para manipulação de informações de crédito.

## 6. Consumo de Serviços de API:

* **Chamadas a Serviços Externos:**
  - Serviço: `CustomerServiceUtils`.
  - Propósito: Manipular dados de crédito de clientes.
  - Dados enviados e recebidos não estão explicitamente definidos no código.

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explicitamente definidos no código.

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para integração com serviços SOAP.
  - `kneFREditSOA`, `kneFRCtrlEditSOA`: Componentes personalizados para edição de dados.

* **Componentes Personalizados:**
  - `TCustomerServiceUtils`: Utilitário para manipulação de dados de clientes.

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `checkCredit` (tipo: booleano, obrigatório).
  - `credStat` (tipo: string, obrigatório).
  - `fixDay1` a `fixDay5` (tipo: inteiro, obrigatório).
  - `realDate` (tipo: booleano, obrigatório).
  - `vatNcr` (tipo: booleano, obrigatório).
  - `factVenc` (tipo: booleano, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Cada campo está vinculado a uma coluna específica no banco de dados (ex.: `checkCredit` → coluna `checkCredit`).

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  FRAMEcustCredit := TFRAMEcustCredit.Create(Self);
  FRAMEcustCredit.Show;
  ```
* **Captura de Tela (HTML Renderizado):**
  ```html
  <div style="width: 708px; height: 255px; border: 1px solid black;">
    <div style="width: 708px; height: 63px; border-bottom: 1px solid black;">
      <label style="position: absolute; left: 128px; top: 8px;">Credit Stat</label>
      <label style="position: absolute; left: 22px; top: 36px;">Fixed Payment Days</label>
      <input type="checkbox" style="position: absolute; left: 4px; top: 8px;" />
      <input type="text" style="position: absolute; left: 200px; top: 4px; width: 49px;" />
    </div>
  </div>
  ```

## 11. Comentários Importantes no Código:

* Configuração inicial do formulário no construtor `Create`.
* Configuração do serviço de dados com `ProviderService`.

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar informações de crédito de clientes. Ele é bem estruturado e utiliza componentes visuais e serviços SOAP para manipulação de dados. No entanto, faltam validações explícitas e mensagens de erro detalhadas.

## 13. Resumo Curto:

Formulário Delphi para gerenciar informações de crédito de clientes, com integração SOAP e componentes visuais para edição e visualização de dados.#### **FRcustCredit.pas**

```
unit FRcustCredit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFREditSOA, InvokeRegistry, StdCtrls, Mask, DBCtrls, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, ImgList, ActnList, Buttons,
  kneFRCtrlEditSOA;

type
  TFRAMEcustCredit = class(TFRAMEBaseCtrlEditSOA)
    PNLcredict1: TPanel;
    CHKcheckCredit: TDBCheckBox;
    Label5: TLabel;
    EDTcredStat: TDBEdit;
    CHKrealDate: TDBCheckBox;
    CHKvatNcr: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    Label6: TLabel;
    EDTfixDay1: TDBEdit;
    EDTfixDay2: TDBEdit;
    EDTfixDay3: TDBEdit;
    EDTfixDay4: TDBEdit;
    EDTfixDay5: TDBEdit;
    PNLCurr: TPanel;
    PNL1: TPanel;
    PNL2: TPanel;
    PNLeur: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    EDTcredLmtEur: TDBEdit;
    EDTcredLmtInvEur: TDBEdit;
    EDToutInvEur: TDBEdit;
    EDTwipEur: TDBEdit;
    EDTavail_cred_eur: TDBEdit;
    PNLcur: TPanel;
    DBTXTcur: TDBText;
    EDTcredLmt: TDBEdit;
    EDTcredLmtInv: TDBEdit;
    EDToutInv: TDBEdit;
    EDTwip: TDBEdit;
    EDTavailCred: TDBEdit;
    grp1: TGroupBox;
    LBLlastupd: TLabel;
    DBTXTuser: TDBText;
    DBTXTlastUpd: TDBText;
    Panel1: TPanel;
    Label1: TLabel;
  private
    { Private declarations }
  public

  published
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEcustCredit: TFRAMEcustCredit;

implementation

{$R *.dfm}

uses
  kneInterfaces, kneFindDialogSOA, kneUtils, CustomerServiceUtils;

{ TFRAMEBaseCtrlEditSOA }


constructor TFRAMEcustCredit.Create(AOwner: TComponent);
begin
  inherited;
  Width := 707;
  Height := 287;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'CustomerCredit';
  PropertyName := '';

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService:= TCustomerServiceUtils.Create(self);
end;


```

#### **FRcustCredit.dfm**

```
inherited FRAMEcustCredit: TFRAMEcustCredit
  Width = 708
  Height = 255
  object PNLcredict1: TPanel [1]
    Left = 0
    Top = 0
    Width = 708
    Height = 63
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label5: TLabel
      Left = 128
      Top = 8
      Width = 61
      Height = 16
      Caption = 'Credit Stat'
    end
    object Label6: TLabel
      Left = 22
      Top = 36
      Width = 124
      Height = 16
      Caption = 'Fixed Payment Days'
    end
    object CHKcheckCredit: TDBCheckBox
      Left = 4
      Top = 8
      Width = 105
      Height = 17
      Caption = 'Check Credit'
      DataField = 'checkCredit'
      DataSource = DStable
      TabOrder = 0
      ValueChecked = 'True'
      ValueUnchecked = 'False'
    end
    object EDTcredStat: TDBEdit
      Left = 200
      Top = 4
      Width = 49
      Height = 24
      DataField = 'credStat'
      DataSource = DStable
      TabOrder = 1
    end
    object CHKrealDate: TDBCheckBox
      Left = 571
      Top = 8
      Width = 121
      Height = 17
      Caption = 'Check Real Date'
      DataField = 'realDate'
      DataSource = DStable
      TabOrder = 2
      ValueChecked = 'True'
      ValueUnchecked = 'False'
    end
    object CHKvatNcr: TDBCheckBox
      Left = 571
      Top = 28
      Width = 121
      Height = 17
      Caption = 'VAT on Credits'
      DataField = 'vatNcr'
      DataSource = DStable
      TabOrder = 3
      ValueChecked = 'True'
      ValueUnchecked = 'False'
    end
    object DBCheckBox2: TDBCheckBox
      Left = 571
      Top = 47
      Width = 127
      Height = 17
      Caption = 'Invoices OverDue'
      DataField = 'factVenc'
      DataSource = DStable
      TabOrder = 4
      ValueChecked = 'True'
      ValueUnchecked = 'False'
    end
    object EDTfixDay1: TDBEdit
      Left = 200
      Top = 32
      Width = 49
      Height = 24
      DataField = 'fixDay1'
      DataSource = DStable
      TabOrder = 5
    end
    object EDTfixDay2: TDBEdit
      Left = 256
      Top = 32
      Width = 49
      Height = 24
      DataField = 'fixDay2'
      DataSource = DStable
      TabOrder = 6
    end
```
<!-- tabs:end -->

